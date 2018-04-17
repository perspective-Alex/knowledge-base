RESOURCES_DIR = ./resources/Mystem
CONTENT_FILE = ./data/astronomy2.txt
MORPH_FILE = ./data/mf_astronomy2.txt
OUT_FILE = ./output.txt

mf:
	$(RESOURCES_DIR)/mystem -n -i -d $(CONTENT_FILE) $(MORPH_FILE)
build:
	stack build
exec:
	stack exec knBase $(CONTENT_FILE) $(MORPH_FILE) $(OUT_FILE)

all: mf build exec 
