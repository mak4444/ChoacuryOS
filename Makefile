BUILD_DIR := build
SRC_DIR   := src

CC  := gcc
CXX := g++ # C++
LD  := ld
ASM := nasm

CFLAGS   := -m32 -O2 -lto -static -fPIC -fstack-protector -ffreestanding -Wall -Wextra -I$(SRC_DIR)
CXXFLAGS := $(CFLAGS) -fno-exceptions -fno-rtti
ASMFLAGS := -f elf32
LDFLAGS  := -m elf_i386 -T $(SRC_DIR)/linker.ld -nostdlib -flto

# we should wildcard this #
SRCS :=								\
	drivers/debug.c					\
	drivers/filesystem/fat.c		\
	drivers/gdt.c					\
	drivers/idt.c					\
	drivers/interrupt.asm			\
	drivers/key.c					\
	drivers/pci.c					\
	drivers/pic.c					\
	drivers/pit.c					\
	drivers/ports.c					\
	drivers/ps2_keyboard.c			\
	drivers/ps2_mouse.c				\
	drivers/keymaps/ps2_keymap_fi.c	\
	drivers/keymaps/ps2_keymap_us.c	\
	drivers/ps2.c					\
	drivers/sound.c					\
	drivers/storage/ata.c			\
	drivers/storage/device.c		\
	drivers/storage/gpt.c			\
	drivers/storage/partition.c		\
	drivers/ssp.c					\
	drivers/utils.c					\
	drivers/vga.c					\
	drivers/vbe.c					\
	drivers/GPU/vmsvga/vmsvga.c		\
	kernel/kernel.c					\
	kernel/krnentry.asm				\
	kernel/panic.c					\
	memory/kmalloc.c				\
	memory/pmm.c					\
	shell/shell.c					\
	shell/terminal.c				\
	gui/desktop.cpp					\
	shell/commands/command.c        \
	shell/commands/guiload/guiload.c \
	shell/commands/clear/clear.c    \
	shell/commands/beep/beep.c      \
	shell/commands/calc/calc.c      \
	shell/commands/compdate/compdate.c \
	shell/commands/echo/echo.c      \
	shell/commands/forth/forth.c    \
	shell/commands/pause/pause.c    \
	shell/commands/pl/pl.c          \
	shell/commands/chstat/chstat.c  \
	shell/commands/cd/cd.c          \
	shell/commands/cat/cat.c        \
	shell/commands/ls/ls.c          \
	shell/commands/whereami/whereami.c \
	shell/commands/vbetest/vbetest.c \
	shell/commands/recovery/recovery.c \
	gui/bitmap/bitmap.c             \
	gui/window/window.cpp           \
	gui/window/manager/manager.cpp  \
	gui/window/gui.cpp              \
	recovery/recovery.cpp          	\
	recovery/pong.cpp               \
	#gui/widgets.c					\ # remove comment when its implemented.
# gui/desktop.cpp (See line 46) was previously a C file
# gui/window/window.cpp (See line 65) was previously a C file
# gui/window/manager/manager.cpp (See line 66) was previously a C file

OBJS := $(addprefix $(BUILD_DIR)/,$(addsuffix .o,$(SRCS)))
DEPS := $(addprefix $(BUILD_DIR)/,$(addsuffix .d,$(filter-out %.asm,$(SRCS))))

.PHONY: all kernel iso run clean

all: kernel

kernel: $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) -o $(BUILD_DIR)/ChoacuryOS.bin
	grub-file --is-x86-multiboot $(BUILD_DIR)/ChoacuryOS.bin	

img: kernel
	./create-disk.sh

run: img
	qemu-system-x86_64 -hda $(BUILD_DIR)/ChoacuryOS.img -serial stdio -audiodev pa,id=snd0 -machine pcspk-audiodev=snd0 -hdb  fat:rw:../disk_efi
	echo qemu-system-x86_64 -hda $(BUILD_DIR)/ChoacuryOS.img -serial stdio -audiodev pa,id=snd0 -machine pcspk-audiodev=snd0

clean:
	rm -rf $(BUILD_DIR)

-include $(DEPS)

$(BUILD_DIR)/%.c.o: $(SRC_DIR)/%.c Makefile
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

$(BUILD_DIR)/%.cpp.o: $(SRC_DIR)/%.cpp Makefile
	@mkdir -p $(@D)
	$(CXX) $(CXXFLAGS) -MMD -MP -c $< -o $@

$(BUILD_DIR)/%.asm.o: $(SRC_DIR)/%.asm
	@mkdir -p $(@D)
	$(ASM) $(ASMFLAGS) $< -o $@
