RESOURCES_DIR = ./resources
CONTENT_FILE = ./data/ex2.txt
MORPH_FILE = ./data/mf2.txt

mf:
	$(RESOURCES_DIR)/mystem -n -i -d $(CONTENT_FILE) $(MORPH_FILE)
build:
	stack build
exec:
	stack exec knBase $(CONTENT_FILE) $(MORPH_FILE)

all: mf build exec 
