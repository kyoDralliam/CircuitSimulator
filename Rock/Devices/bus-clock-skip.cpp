#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <ncurses.h>
#include <time.h>
#include <pthread.h>
#include <string.h>
#include <csignal>
#include <errno.h>

#include "device.h"

namespace circuit
{
  void stop();
}

namespace
{
  class bus_cons;
}

class Clock_bus_reader;

class Clock_bus : public device
{
public :
  Clock_bus(int id, int size, int number_of_displays, int frequency);
  virtual ~Clock_bus();
  virtual unsigned int cycle (unsigned int address, unsigned int data,
                      char byte_enables, bool write_enable,
                      bool interrupt_enable, bool iack, char * irq);
  
  static device * make(int id, int size, int set_number_of_displays,
                       int frequency);

  void draw(WINDOW * window);
  
  unsigned int memory_access (unsigned int address, unsigned int data,
                              char byte_enables, bool write_enable);

private :
  char * mem;
  unsigned int mem_size;
  
  char * displays_mem;
  int number_of_displays; // == -1 : Clock
  bus_cons * cons;

  time_t timestamp;
  int ticks;
  int frequency;

  int id;

  unsigned int * mod_ptr;
  unsigned int mod_val;
  unsigned int mod_addr;
  bool wrote;

  friend class Clock_bus_reader;
};


class Clock_bus_reader : public device
{
public :
  Clock_bus_reader(int id);
  virtual ~Clock_bus_reader();
  virtual unsigned int cycle (unsigned int address, unsigned int data,
                      char byte_enables, bool write_enable,
                      bool interrupt_enable, bool iack, char * irq);
  
  static device * make(int id);
  
  

private :
  Clock_bus * target;
  int id;
  time_t timestamp;
  int ticks;
};


namespace
{

  class bus_cons
  {
  public:
    Clock_bus * bus;
    bus_cons * next;
    
    bus_cons (Clock_bus * bus);
    ~bus_cons ();
  };

  bool ncurses_started = false;
  bool stopping = false;
  
  bus_cons * buses_list = NULL;
  
  time_t start_timestamp;
  bool start_timestamp_initialized = false;

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

  bool file_slurp (char * file_name, char * buffer, int buffer_size)
  {
    FILE *file;
    int length;
    int read_length;
    file = fopen(file_name, "rb");
    if (file == NULL) return errno == EACCES;
    fseek(file, 0, SEEK_END);
    length = ftell(file);
    rewind(file);
    if (length > buffer_size) { fclose(file); return false; }
    read_length = fread(buffer, length, 1, file);
    fclose(file);
    file = NULL;
    return read_length == 1;
  }

  char ram_file[] = "ram";
}

Clock_bus::Clock_bus(int id, int size, int number_of_displays, int frequency)
{
  if (size < 2 || size > 28 ||
      number_of_displays < -1 || number_of_displays > 10)
    {
      fatal();
    }

  this->id = id;

  if (!start_timestamp_initialized)
   {
     start_timestamp = time(NULL);
     start_timestamp_initialized = true;
   }
  timestamp = start_timestamp;
  ticks = 0;
  this->frequency = frequency;

  mem_size = 1<<size;
  mem = new char[mem_size];
  
  this->number_of_displays = number_of_displays;
  
  wrote = false;

  if (number_of_displays > 0)
   {
     displays_mem = new char[number_of_displays * 16];
     memset (displays_mem, 0, number_of_displays * 16);
   }
  else if (number_of_displays == -1)
   {
     displays_mem = new char[14];
     memset (displays_mem, 0, 14);
   }
  else
   {
     displays_mem = NULL;
   }
   
  if (!file_slurp(ram_file, mem, mem_size)) fatal();

  cons = new bus_cons(this);
}

Clock_bus::~Clock_bus()
{
  delete[] mem;
  delete[] displays_mem;
  delete cons;
}
  
device * Clock_bus::make(int id, int size, int number_of_displays, int frequency)
{
  return dynamic_cast<device *>(new Clock_bus(id, size, number_of_displays,
                                              frequency));
}

unsigned int Clock_bus::cycle (unsigned int address, unsigned int data,
                            char byte_enables, bool write_enable,
                            bool interrupt_enable, bool iack, char * irq)
{
  unsigned int r = memory_access(address, data, byte_enables, write_enable);
  if (++ticks == frequency) {ticks = 0; timestamp+=100;}
  wrote = write_enable;
  return r;
}


unsigned int Clock_bus::memory_access (unsigned int address, unsigned int data,
                                       char byte_enables, bool write_enable)
{
  unsigned int r;
  unsigned int mask = ((byte_enables & 1) ? 0x000000FF : 0)
    | ((byte_enables & 2) ? 0x0000FF00 : 0)
    | ((byte_enables & 4) ? 0x00FF0000 : 0)
    | ((byte_enables & 8) ? 0xFF000000 : 0);
  
  if (address & 0x80000000)
    {
     if ((address & 0xFFFFF000) == 0x80001000)
       {
         if (write_enable) fatal();
         r = timestamp;
       }
     else if ((address & 0xFFFFF000) == 0x80000000)
       {
         if (number_of_displays == -1)
           {
             if ((address & 0xFFF) + 4 > 14) fatal();
           }
         else
           {
             if ((address & 0xFFF) + 4 > number_of_displays * 16) fatal();
           }

         r = *(reinterpret_cast<unsigned int *>(displays_mem + (address & 0xFFF)));
         
         if (write_enable)
           {
             mod_addr = address;
             mod_ptr = (reinterpret_cast<unsigned int *>(displays_mem + (address & 0xFFF)));
             mod_val = r;
             *(reinterpret_cast<unsigned int *>(displays_mem + (address & 0xFFF))) =
             (data & mask) | (r & ~mask);
           }
       }
     else fatal();
    }
  else
   {
     if (address > mem_size - 4) fatal();
        
     r = *(reinterpret_cast<unsigned int *>(mem + address));
  
     if (write_enable)
      {
        mod_addr = address;
        mod_ptr = (reinterpret_cast<unsigned int *>(mem + address));
        mod_val = r;
        *(reinterpret_cast<unsigned int *>(mem + address)) =
          (data & mask) | (r & ~mask);
      }
   }

  return r & mask;  
}

void Clock_bus::draw(WINDOW * window)
 {
   int i; int j; int k; int x; int y;
   bool s[7];

   if (number_of_displays == -1)
    {
      for (j = 0; j < 14; j++)
       {
         for (k = 0; k < 7; k++)
          {
            s[k] = displays_mem[j] & (1 << k);
          }
         if (j < 2) { x = 2 + j * 7; y = 1; }
         else if (j < 4)  { x = 5 + j * 7; y = 1; }
         else if (j < 6)  { x = 8 + j * 7; y = 1; }
         else if (j < 8)  { x = -40 + j * 7; y = 10; }
         else if (j < 10) { x = -33 + j * 7; y = 10; }
         else { x = -26 + j * 7; y = 10; }

         wprint_config (window, y, x, s[0],
                        s[1], s[2], s[3], s[4], s[5], s[6]);
       }
      mvwaddstr(window, 3, 16, "#");
      mvwaddstr(window, 5, 16, "#");
      mvwaddstr(window, 3, 33, "#");
      mvwaddstr(window, 5, 33, "#");
      
      for (i = 0; i < 7; i++)
       {
         mvwaddstr(window, 16-i, 15+i, "/");
         mvwaddstr(window, 16-i, 36+i, "/");
       }
    }
   else
    {
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
 }

Clock_bus_reader::Clock_bus_reader(int id)
{
  if (id < 0)
   {
     fatal();
   }

  if (!start_timestamp_initialized)
   {
     start_timestamp = time(NULL);
     start_timestamp_initialized = true;
   }
  timestamp = start_timestamp;
  ticks = 0;

  this->id = id;
  target = NULL;
}

Clock_bus_reader::~Clock_bus_reader() {}
  
device * Clock_bus_reader::make(int id)
{
  return dynamic_cast<device *>(new Clock_bus_reader(id));
}

unsigned int Clock_bus_reader::cycle (unsigned int address, unsigned int data,
                                      char byte_enables, bool write_enable,
                                      bool interrupt_enable, bool iack,
                                      char * irq)
{
  unsigned int r;

  if (target == NULL)
   {
     bus_cons * curr;
     pthread_mutex_lock(&ncurses_mutex);
     for (curr = buses_list; curr != NULL; curr = curr->next)
      {
        if (curr->bus->id == this->id)
         {
           target = curr->bus;
           break;
         }
      }
     pthread_mutex_unlock(&ncurses_mutex);

     if (target == NULL) fatal();
   }
   
  if ( (address & 0xFFFFF000) == 0x1000)
   {
     r = timestamp;
   }
  else if ( address + 3 < target->mod_addr || address > target->mod_addr + 3
            || !target->wrote || (ticks == target->ticks
            && timestamp == target->timestamp) )
   {
     r = target->memory_access(address, data, byte_enables, false);
   }
  else
   {
     unsigned int saved = *target->mod_ptr;
     *target->mod_ptr = target->mod_val;
     r = target->memory_access(address, data, byte_enables, false);
     *target->mod_ptr = saved;
   }

  if (++ticks == target->frequency) {ticks = 0; timestamp++;}
  return r;
}

namespace
{
  bus_cons::bus_cons(Clock_bus * bus)
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
    return 0;
  }
}
