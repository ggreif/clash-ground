
* How To build an SPI Flash image? *

rm boot.bif
echo "the_ROM_image:" >> boot.bif
echo "{" >> boot.bif
echo "<pl_bitstream_name>.bit" >> boot.bif
echo "}" >> boot.bif

cat boot.bif

/home/ggreif/.CADAPPL/.caddata/xilinx/SDK/bin/bootgen -image boot 


* How To convert a freestanding .bit to a .bit.bin? *

rm all.bif
echo "all:" > all.bif
echo "{" >> all.bif
echo "v232/RS232.runs/impl_2/rs232_tx.bit" >> all.bif
echo "}" >> all.bif

cat all.bif

# NO! promgen -p bin -data_width 32 -b -u 0x0 <design_name>.bit

/home/ggreif/.CADAPPL/.caddata/xilinx/SDK/bin/bootgen -image all.bif -w -process_bitstream bin


* How To mount the FAT partition when in Linux? *
$ mkdir -p /media/card && mount -t msdos /dev/mmcblk0p1 /media/card


* How To pump the FPGA from Linux? *
$ cat /media/card/rs232_~1.bin > /dev/xdevcfg

