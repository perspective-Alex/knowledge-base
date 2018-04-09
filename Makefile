RESOURCES_DIR = ./resources
CONTENT_FILE = ./data/books/oki2.txt
MORPH_FILE = ./data/books/mf_oki2.txt

mf:
	$(RESOURCES_DIR)/mystem -n -i -d $(CONTENT_FILE) $(MORPH_FILE)
build:
	stack build
exec:
	stack exec knBase $(CONTENT_FILE) $(MORPH_FILE)

all: mf build exec 
