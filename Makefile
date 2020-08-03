ROMNAME := smurfed
HEADERNAME := "MAKRO19"

RGBPATH := /usr/bin

all: $(ROMNAME).gb

$(ROMNAME).gb: obj/$(ROMNAME).o
	$(RGBPATH)/rgblink -n $(ROMNAME).sym -o $(ROMNAME).gb obj/$(ROMNAME).o
	$(RGBPATH)/rgbfix -v -t $(HEADERNAME) $(ROMNAME).gb

obj/$(ROMNAME).o: $(ROMNAME).asm inc/* art/ascii.2bpp art/intro_1.2bpp art/lettering.2bpp art/logo.2bpp
	[ -d obj ] || mkdir obj
	$(RGBPATH)/rgbasm -h -o obj/$(ROMNAME).o $(ROMNAME).asm

art/%.2bpp: art/%.png
	$(RGBPATH)/rgbgfx -o $@ $<

clean:
	rm -r obj
	rm *.sym *.gb
	rm -f art/*.2bpp
