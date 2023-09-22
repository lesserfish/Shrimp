import time
import os
os.environ['PYGAME_HIDE_SUPPORT_PROMPT'] = "hide"
import pygame
import sys

filepath = "./Roms/nestest.nes"
filepath = "./Roms/Super_mario_brothers.nes"

# Get ROM data
print("Getting ROM Data...")
file = open(filepath, "rb")

initial_position = file.tell()
file.seek(0, 2)  # Move to the end of the file
total_size = file.tell()
file.seek(initial_position)

magic_number = file.read(4)
prg_size = file.read(1)[0]
chr_size = file.read(1)[0]
flag6 = file.read(1)[0]
mirroring = flag6 & 0b00000001
battery =   flag6 & 0b00000010
trainer =   flag6 & 0b00000100
fsvram =    flag6 & 0b00001000
lmapper =   (flag6 & 0xF0) >> 4
flag7 = file.read(1)[0]
vs        = flag7 & 0b00000001
pc10      = flag7 & 0b00000010
nes2f     = (flag7 & 0b00001100) == 2
umapper =  (flag7 & 0xF0)
mapper = lmapper + umapper
flag8 = file.read(1)[0]
prg_ram_size = flag8
flag9 = file.read(1)[0]
tvsystem = ""
if flag9 & 0x01 == 0:
    tvsystem = "NTSC"
else: 
    tvsystem = "PAL"
flag10 = file.read(1)[0]
tvsystem2r = flag10 & 0x00000011
tvsystem2 = ""
if tvsystem2r == 0:
    tvsystem2 = "NTSC"
elif tvsystem2r == 1:
    tvsystem2 = "PAL"
else:
    tvsystem2 = "Dual"
prg_ram_present = flag10 & 0b00010000
bus_conflict = flag10 & 0b00100000
padding = file.read(5)
print("Magic Number: {}".format(magic_number))
print("PRG Rom size: {}".format(prg_size))
print("CHR Rom size: {}".format(chr_size))
print("Mirroring: {}".format(mirroring))
print("Battery: {}".format(battery))
print("Trainer: {}".format(trainer))
print("Provide Four-Way Screen VRAM: {}".format(fsvram))
print("VS Unisystem: {}".format(vs))
print("PlayChoice-10 : {}".format(pc10))
print("Use NES2.0 format : {}".format(nes2f))
print("Mapper: {}".format(mapper))
print("PRG Ram size: {}".format(prg_ram_size))
print("TV System: {}".format(tvsystem))
print("TV System 2: {}".format(tvsystem2))
print("PRG Ram present: {}".format(prg_ram_present))
print("Bus conflict: {}".format(bus_conflict))

trainer_data = ""
if trainer == 1:
    trainer_data = file.read(512)

prg_data = file.read(16 * 1024 * prg_size)
chr_data = file.read(8 * 1024 * chr_size)

current_position = file.tell()
print("Bytes left in ROM: {}".format(total_size - current_position))
file.close()

## Handling the data


w = 16 * 8 * 2
h = 16 * 8
scale_factor = 4

# Boiler plate clode
screen_width = w * scale_factor
screen_height = h * scale_factor

pygame.display.set_caption("")
pygame.init()
screen = pygame.display.set_mode((screen_width, screen_height))
image_surface = pygame.Surface((w, h))

pixcolor = [(255, 255, 255), (122, 122, 122), (0, 0, 0), (255, 0, 0)]

for table_idx in range(0, 2):
    for tilex in range(0, 16):
        for tiley in range(0, 16):
            tile_offset = 256 * tiley + 16 * tilex
            for px in range(0, 8):
                tile_lsb = chr_data[table_idx * 0x1000 + tile_offset + px + 0x0000]
                tile_hsb = chr_data[table_idx * 0x1000 + tile_offset + px + 0x0008]

                for py in range(0, 8):
                    pixel = ((tile_lsb >> py) & 0x01) + ((tile_hsb >> py) & 0x01)

                    x = tilex * 8 + (7 - py) + table_idx * 16*8
                    y = tiley * 8 + px
                    image_surface.set_at((x, y), pixcolor[pixel])

# Render

scaled_image = pygame.transform.scale(image_surface, (w * scale_factor, h * scale_factor))

# Main loop
running = True
while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False
    screen.fill((0, 0, 0))
    screen.blit(scaled_image, (0, 0))
    pygame.display.flip()

# Quit Pygame
pygame.quit()
sys.exit()
