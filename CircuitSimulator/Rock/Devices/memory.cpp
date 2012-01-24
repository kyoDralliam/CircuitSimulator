#include <stdlib.h>
#include <stdio.h>
#include <csignal>
#include <errno.h>

#include "device.h"

namespace
{
  class memory_cons;
}

class Memory_reader;

class Memory : public device
{
public :
  Memory(int id, int size, int init_ram);
  virtual ~Memory();
  virtual unsigned int cycle (unsigned int address, unsigned int data,
                      char byte_enables, bool write_enable,
                      bool interrupt_enable, bool iack, char * irq);
  
  static device * make(int id, int size, int init_ram);

  unsigned int memory_access (unsigned int address, unsigned int data,
                              char byte_enables, bool write_enable);

private :
  char * mem;
  unsigned int mem_size;
  
  memory_cons * cons;

  int current_time;

  int id;

  unsigned int * mod_ptr;
  unsigned int mod_val;
  unsigned int mod_addr;
  bool wrote;

  friend class Memory_reader;
};

class Memory_reader : public device
{
public :
  Memory_reader(int id);
  virtual ~Memory_reader();
  virtual unsigned int cycle (unsigned int address, unsigned int data,
                      char byte_enables, bool write_enable,
                      bool interrupt_enable, bool iack, char * irq);
  
  static device * make(int id);
  
  
private :
  Memory * target;
  int id;
  int current_time;
};


namespace
{

  class memory_cons
  {
  public:
    Memory * memory;
    memory_cons * next;
    
    memory_cons (Memory * memory);
    ~memory_cons ();
  };
  
  memory_cons * memories_list = NULL;
  
  void fatal ();

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

Memory::Memory(int id, int size, int init_ram)
{
  if (size < 2 || size > 28)
    {
      fatal();
    }

  this->id = id;

  mem_size = 1<<size;
  mem = new char[mem_size];
  
  current_time = 0;
  
  wrote = false;

  if (init_ram)
    {
      if (!file_slurp(ram_file, mem, mem_size)) fatal();
    }

  cons = new memory_cons(this);
}

Memory::~Memory()
{
  delete[] mem;
  delete cons;
}
  
device * Memory::make(int id, int size, int init_ram)
{
  return dynamic_cast<device *>(new Memory(id, size, init_ram));
}

unsigned int Memory::cycle (unsigned int address, unsigned int data,
                            char byte_enables, bool write_enable,
                            bool interrupt_enable, bool iack, char * irq)
{
  unsigned int r = memory_access(address, data, byte_enables, write_enable);
  current_time++;
  wrote = write_enable;
  return r;
}


unsigned int Memory::memory_access (unsigned int address, unsigned int data,
                                       char byte_enables, bool write_enable)
{
  unsigned int r;
  unsigned int mask = ((byte_enables & 1) ? 0x000000FF : 0)
    | ((byte_enables & 2) ? 0x0000FF00 : 0)
    | ((byte_enables & 4) ? 0x00FF0000 : 0)
    | ((byte_enables & 8) ? 0xFF000000 : 0);
  
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

  return r & mask;  
}

Memory_reader::Memory_reader(int id)
{
  if (id < 0)
   {
     fatal();
   }

  current_time = 0;
 
  this->id = id;
  target = NULL;
}

Memory_reader::~Memory_reader() {}
  
device * Memory_reader::make(int id)
{
  return dynamic_cast<device *>(new Memory_reader(id));
}

unsigned int Memory_reader::cycle (unsigned int address, unsigned int data,
                                      char byte_enables, bool write_enable,
                                      bool interrupt_enable, bool iack,
                                      char * irq)
{
  unsigned int r;

  if (target == NULL)
   {
     memory_cons * curr;
     for (curr = memories_list; curr != NULL; curr = curr->next)
      {
        if (curr->memory->id == this->id)
         {
           target = curr->memory;
           break;
         }
      }

     if (target == NULL) fatal();
   }

  if (address + 3 < target->mod_addr || address > target->mod_addr + 3
      || !target->wrote || current_time == target->current_time)
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
 
  current_time++;
  return r;
}

namespace
{
  memory_cons::memory_cons(Memory * memory)
  {
    next = memories_list;
    this->memory = memory;
    memories_list = this;
  }

  memory_cons::~memory_cons() 
  {
    memory_cons ** ref;
     
    ref = &memories_list;
    while (*ref && *ref != this) ref = &(*ref)->next;
    if (*ref == this) *ref = this->next;
  }

  void fatal ()
  {
    raise(SIGINT);
  }
}
