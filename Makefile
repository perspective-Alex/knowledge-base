MYSTEM_DIR = ./resources/Mystem
TOMITA_DIR = ./resources/Tomita
RESULTS_DIR = ./results
CONTENT_FILE1 = ./data/oki4.txt
CONTENT_FILE2 = ./data/examples/bio4.txt
CONTENT_FILE3 = ./data/examples/bio3.txt
MORPH_FILE1 = ./data/mf_oki4.txt
MORPH_FILE2 = ./data/examples/mf_bio4.txt
MORPH_FILE3 = ./data/examples/mf_bio3.txt
OUT_FILE = ./output.txt

CONTENTS = $(CONTENT_FILE1)
MORPHS = $(MORPH_FILE1)

mf:
	$(MYSTEM_DIR)/mystem -n -i -d $(CONTENT_FILE1) $(MORPH_FILE1)
build:
	stack build
exec:
	stack exec knBase $(OUT_FILE) $(CONTENTS) $(MORPHS) >> \
	$(RESULTS_DIR)/out/out5/res.txt
	cd $(TOMITA_DIR) ; \
	sed -i 's/\"//g' ./keywords.txt ; \
	./tomita-parser config_def.proto ; \
  ./tomita-parser config_rel.proto ;
	cp $(TOMITA_DIR)/*.html $(RESULTS_DIR)/out/out5

exec_simple:
	stack exec knBase $(OUT_FILE) $(CONTENTS) $(MORPHS)
	cd $(TOMITA_DIR) ; \
	sed -i 's/\"//g' ./keywords.txt ; \
	./tomita-parser config_def.proto ; \
  ./tomita-parser config_rel.proto ;

pre:
	sed -i ':a; /$$/N; s/\- \{0,3\}\n//; ta' $(CONTENTS)
	sed -i ':a; /$$/N; s/\n/ /; ta' $(CONTENTS)
	cat $(CONTENTS) >> $(TOMITA_DIR)/input1.txt

clean: 
	echo "" > $(TOMITA_DIR)/input1.txt

all: clean pre mf build exec 

simple: clean pre mf build exec_simple
