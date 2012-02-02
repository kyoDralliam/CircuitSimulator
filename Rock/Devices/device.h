class device
{
public :
  virtual ~device();
  virtual unsigned int cycle (unsigned int address, unsigned int data,
                              char byte_enables, bool write_enable,
                              bool interrupt_enable, bool iack, char * irq)=0;
};
