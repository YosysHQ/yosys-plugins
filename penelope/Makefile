
PENELOPE_VER = 08b284b059bf
PENELOPE_OBJS = Asserts.o Cooperation.o INIParser.o Options.o Semaphore.o Solver.o System.o

penelope.so: $(PENELOPE_OBJS)
	$(shell yosys-config --cxx --cxxflags -O0 -I bhoessen-penelope-$(PENELOPE_VER)/include --ldflags -o penelope.so -shared penelope.cc $(PENELOPE_OBJS) --ldlibs)

bhoessen-penelope-$(PENELOPE_VER): bhoessen-penelope-$(PENELOPE_VER).zip
	unzip bhoessen-penelope-$(PENELOPE_VER).zip
	cd bhoessen-penelope-$(PENELOPE_VER) && patch -p1 < ../penelope-fixes.diff

bhoessen-penelope-$(PENELOPE_VER).zip:
	wget -O bhoessen-penelope-$(PENELOPE_VER).zip.part https://bitbucket.org/bhoessen/penelope/get/$(PENELOPE_VER).zip
	mv bhoessen-penelope-$(PENELOPE_VER).zip.part bhoessen-penelope-$(PENELOPE_VER).zip

$(PENELOPE_OBJS): bhoessen-penelope-$(PENELOPE_VER)
	$(shell yosys-config --cxx -c -o $@ --cxxflags -O0 -I bhoessen-penelope-$(PENELOPE_VER)/include bhoessen-penelope-$(PENELOPE_VER)/src/$(basename $@).cpp)

clean:
	rm -rfv *.d *.o bhoessen-penelope-$(PENELOPE_VER) penelope.so

-include *.d

