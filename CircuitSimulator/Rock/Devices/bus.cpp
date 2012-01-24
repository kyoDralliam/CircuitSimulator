#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <ncurses.h>
#include <time.h>
#include <pthread.h>
#include <string.h>
#include <csignal>

#include "device.h"

namespace circuit
{
  void stop();
}

namespace
{
  class bus_cons;
}

class Bus : public device
{
public :
  Bus(int size, int number_of_displays);
  virtual ~Bus();
  virtual unsigned int cycle (unsigned int address, unsigned int data,
                      char byte_enables, bool write_enable,
                      bool interrupt_enable, bool iack, char * irq);
  
  static device * make(int size, int set_number_of_displays);

  void draw(WINDOW * window);
  
private :
  char * mem;
  unsigned int mem_size;
  
  char * displays_mem;
  int number_of_displays;
  bus_cons * cons;
};

namespace
{

  class bus_cons
  {
  public:
    Bus * bus;
    bus_cons * next;
    
    bus_cons (Bus * bus);
    ~bus_cons ();
  };

  bool ncurses_started = false;
  bool stopping = false;
  
  bus_cons * buses_list = NULL;
  
  pthread_t input_thread;
  pthread_mutex_t ncurses_mutex;
  void * input_function(void * arg);
  
  void start_ncurses ();
  void end_ncurses ();
  void stop ();
  void fatal ();
  
  void switch_window (bool backwards);
  
  void wprint_config (WINDOW * w, int y, int x, bool a, bool b, bool c,
                      bool d, bool e, bool f, bool g);
  
}

Bus::Bus(int size, int number_of_displays)
{
  if (size < 2 || size > 20 ||
      number_of_displays < 0 || number_of_displays > 10)
    {
      fatal();
    }
   
  mem_size = 1<<size;
  mem = new char[mem_size];
  
  this->number_of_displays = number_of_displays;
  
  if (number_of_displays > 0)
   {
     displays_mem = new char[number_of_displays * 16];
     memset (displays_mem, 0, number_of_displays * 16);
   }
  else
   {
     displays_mem = NULL;
   }

  cons = new bus_cons(this);
}

Bus::~Bus()
{
  delete[] mem;
  delete[] displays_mem;
  delete cons;
}
  
device * Bus::make(int size, int number_of_displays)
{
  return dynamic_cast<device *>(new Bus(size, number_of_displays));
}

unsigned int Bus::cycle (unsigned int address, unsigned int data,
                            char byte_enables, bool write_enable,
                            bool interrupt_enable, bool iack, char * irq)
{
  unsigned int r;
  unsigned int mask = ((byte_enables & 1) ? 0x000000FF : 0)
    | ((byte_enables & 2) ? 0x0000FF00 : 0)
    | ((byte_enables & 4) ? 0x00FF0000 : 0)
    | ((byte_enables & 8) ? 0xFF000000 : 0);
  
  if (address & 0x80000000)
    {
     address = address & 0x7FFFFFFF;
     if (address + 4 > number_of_displays * 16) fatal();

     r = *(reinterpret_cast<unsigned int *>(displays_mem + address)) & mask;
  
     *(reinterpret_cast<unsigned int *>(displays_mem + address)) =
       (data & mask) | (r & ~mask);
   }
  else
   {
     if (address > mem_size - 4) fatal();
        
     r = *(reinterpret_cast<unsigned int *>(mem + address)) & mask;
  
     *(reinterpret_cast<unsigned int *>(mem + address)) =
       (data & mask) | (r & ~mask);
   }

  return r;  
}

void Bus::draw(WINDOW * window)
 {
   int i; int j; int k;
   bool s[7];

   for (i = 0; i < number_of_displays; i++)
     {
      for (j = 0; j < 16; j++)
       {
         for (k = 0; k < 7; k++)
          {
            s[k] = displays_mem[i * 16 + j] & (1 << k);
          }
         wprint_config (window, i * 7, j * 6, s[0],
                        s[1], s[2], s[3], s[4], s[5], s[6]);
       }
    }
 }
namespace
{
  bus_cons::bus_cons(Bus * bus)
  {
    if (!ncurses_started) start_ncurses();
    
    pthread_mutex_lock(&ncurses_mutex);

    next = buses_list;
    this->bus = bus;
    buses_list = this;
 
    pthread_mutex_unlock(&ncurses_mutex);
  }

  bus_cons::~bus_cons() 
  {
    bool was_the_last;
    bus_cons ** ref;
     
    pthread_mutex_lock(&ncurses_mutex);

    ref = &buses_list;
    while (*ref && *ref != this) ref = &(*ref)->next;
    if (*ref == this) *ref = this->next;
    was_the_last = !buses_list;
    
    pthread_mutex_unlock(&ncurses_mutex);

    if (was_the_last) stop();
  }

  void switch_window (bool backwards)
  {
    bus_cons * current = NULL;
    bus_cons * saved = NULL;
    bus_cons ** ref = NULL;
    
    if (!buses_list) return;
    
    if (backwards)
      {
        ref = &buses_list;
        for (current = buses_list; current->next; current = current->next)
          ref = &current->next;
        
        *ref = NULL;
        current->next = buses_list;
        buses_list = current;
      }
    else
      {
        saved = buses_list;
        buses_list = saved->next;
        saved->next = NULL;
        for (ref = &buses_list; *ref; ref = &(*ref)->next) {}
        
        *ref = saved;
      }
  }
  
/*
 ___     
|   |      A
|   |    F   B
 ---       G
|   |    E   C
|___|      D
*/

  void start_ncurses ()
  {
    if (ncurses_started) return;
    
    /* Start curses mode */
    initscr();
    clear();
    refresh();
    
    halfdelay(1);            /* Line buffering disabled;
                                getch() returns ERR after 1/10s
                                if no character is typed */
    keypad(stdscr, TRUE);    /* We get F1, F2 etc..*/
    noecho();                /* Don't echo() while we do getch */
    curs_set(0);
    
    ncurses_started = true;
    
    pthread_mutex_init(&ncurses_mutex, NULL);
    pthread_create (&input_thread, NULL, input_function, NULL);
  }
  
  void end_ncurses ()
  {
    if (!ncurses_started) return;
    pthread_mutex_destroy(&ncurses_mutex);
    curs_set(1);
    clear();
    refresh();
    endwin();
    ncurses_started = false;
  }
  
  void stop ()
  {
    pthread_mutex_lock(&ncurses_mutex);
    stopping = true;
    pthread_mutex_unlock(&ncurses_mutex);
    
    pthread_join (input_thread, NULL);
    stopping = false;
    
    end_ncurses();
  }
  
  void fatal ()
  {
    raise(SIGINT);
  }
  
  void wprint_config (WINDOW * w, int y, int x, bool a, bool b, bool c,
                      bool d, bool e, bool f, bool g)
  {
    if (a) mvwaddstr(w, y, x, " ___ ");
    else mvwaddstr(w, y, x, "     ");
    
    if (f) {mvwaddstr(w, y+1, x, "|"); mvwaddstr(w, y+2, x, "|");}
    else {mvwaddstr(w, y+1, x, " "); mvwaddstr(w, y+2, x, " ");}
    
    mvwaddstr(w, y+1, x+1, "   ");
    mvwaddstr(w, y+2, x+1, "   ");
    
    if (b) {mvwaddstr(w, y+1, x+4, "|"); mvwaddstr(w, y+2, x+4, "|");}
    else {mvwaddstr(w, y+1, x+4, " "); mvwaddstr(w, y+2, x+4, " ");}
    if (g) mvwaddstr(w, y+3, x, " --- ");
    else mvwaddstr(w, y+3, x, "     ");
    
    if (e) {mvwaddstr(w, y+4, x, "|"); mvwaddstr(w, y+5, x, "|");}
    else {mvwaddstr(w, y+4, x, " "); mvwaddstr(w, y+5, x, " ");}
    
    mvwaddstr(w, y+4, x+1, "   ");
    
    if (c) {mvwaddstr(w, y+4, x+4, "|"); mvwaddstr(w, y+5, x+4, "|");}
    else {mvwaddstr(w, y+4, x+4, " "); mvwaddstr(w, y+5, x+4, " ");}
    
    if (d) mvwaddstr(w, y+5, x+1, "___");
    else mvwaddstr(w, y+5, x+1, "   ");
  }
  
  void * input_function(void * arg)
  {
    int c;
    
    while (!stopping)
      {
        c = getch();
        
        if (c != ERR)
          {
            pthread_mutex_lock(&ncurses_mutex);
            if (!stopping)
              {
                if (c == 'q')
                  {
                    circuit::stop();
                  }
                else if (c == KEY_LEFT)
                  {
                    switch_window(false);
                  }
              }
            pthread_mutex_unlock(&ncurses_mutex);
          }
        
        pthread_mutex_lock(&ncurses_mutex);
        if (!stopping) 
          {
            if (buses_list && buses_list->bus)
              buses_list->bus->draw(stdscr);
            wrefresh(stdscr);
          }
        pthread_mutex_unlock(&ncurses_mutex);
      }
  }
}
