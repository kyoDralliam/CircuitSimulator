#include <cstdlib>
//#include <cstdio>

class Memory
{
public :
  Memory(int size);
  ~Memory();
  unsigned int cycle (unsigned int address, unsigned int data,
                      char byte_enables, bool write_enable,
                      bool interrupt_enable, bool iack, char * irq);
private :
  char * mem;
  unsigned int mem_size;
};

Memory::Memory(int size)
{
  if (size < 2 || size > 10)
    {
      exit(1);
    }
  
  mem_size = 1<<size;
  mem = new char[mem_size];
}

Memory::~Memory()
{
  delete mem;
}

unsigned int Memory::cycle (unsigned int address, unsigned int data,
                            char byte_enables, bool write_enable,
                            bool interrupt_enable, bool iack, char * irq)
{
  unsigned int r;
  unsigned int mask = ((byte_enables & 1) ? 0x000000FF : 0)
    | ((byte_enables & 2) ? 0x0000FF00 : 0)
    | ((byte_enables & 4) ? 0x00FF0000 : 0)
    | ((byte_enables & 8) ? 0xFF000000 : 0);
  
  //printf("\t\t%d\n", data);

  if (address > mem_size - 4) exit(1);

  r = *(reinterpret_cast<unsigned int *>(mem + address)) & mask;
  
  *(reinterpret_cast<unsigned int *>(mem + address)) =
    (data & mask) | (r & ~mask);

  return r;  
}
