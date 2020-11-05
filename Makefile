EMACS=emacs

.PHONY: test package elpa clean build-keywords compile-test

package: *.el
	@ver=`grep -o "Version: .*" maxima.el | cut -c 10-`; \
	tar czvf maxima-$$ver.tar.gz --mode 644 $$(find . -name \*.el)

elpa: *.el
	@version=`grep -o "Version: .*" maxima.el | cut -c 10-`; \
	dir=maxima-$$version; \
	mkdir -p "$$dir"; \
	cp $$(find . -name \*.el) maxima-$$version; \
	echo "(define-package \"maxima\" \"$$version\" \
	\"Modular in-buffer completion framework\")" \
	> "$$dir"/maxima-pkg.el; \
	tar cvf maxima-$$version.tar --mode 644 "$$dir"

clean:
	@rm -rf maxima-*/ maxima-*.tar maxima-*.tar.bz2 *.elc ert.el .elpa/

make-test:
	${EMACS}  --batch -l test/make-install.el -l test/make-test.el 

test: make-test clean

build-keywords:
	awk '/^ -- Function/' keywords/keywords.txt > keywords/functions
	cd keywords && awk '/^ -- Function:/{print $$3}' functions > functions2 && rm functions && mv functions2 functions

	awk '/^ -- Constant/' keywords/keywords.txt > keywords/constants
	 cd keywords && awk '/^ -- Constant/{print $$3}' constants > constants2 && rm constants && mv constants2 constants

	awk '/^ -- Operator/' keywords/keywords.txt > keywords/operators
	 cd keywords && awk '/^ -- Operator/{print $$3}' operators > operators2 && rm operators && mv operators2 operators

	awk '/^ -- Option/' keywords/keywords.txt > keywords/options
	 cd keywords && awk '/^ -- Option/{print $$4}' options > options2 && rm options && mv options2 options

	awk '/^ -- Plot/' keywords/keywords.txt > keywords/plot
	 cd keywords && awk '/^ -- Plot/{print $$4}' plot > plot2 && rm plot && mv plot2 plot

	awk '/^ -- Graphic/' keywords/keywords.txt > keywords/graphic
	 cd keywords && awk '/^ -- Graphic/{print $$4}' graphic > graphic2 && rm graphic && mv graphic2 graphic 

	awk '/^ -- draw_/' keywords/keywords.txt >> keywords/graphic 
	 cd keywords && awk '/^ -- draw_/{print $$3}' graphic >> graphic 

	awk '/^ -- System/' keywords/keywords.txt > keywords/system_variables
	 cd keywords && awk '/^ -- System/{print $$4}' system_variables > system_variables2 && rm system_variables && mv system_variables2 system_variables

	awk '/^ -- Scene/' keywords/keywords.txt > keywords/scene
	 cd keywords && awk '/^ -- Scene/{print $$4}' scene > scene2 && rm scene && mv scene2 scene

	awk '/^ -- Object/' keywords/keywords.txt > keywords/object
	 cd keywords && awk '/^ -- Object/{print $$4}' object > object2 && rm object && mv object2 object

	awk '/^ -- Global/' keywords/keywords.txt > keywords/global
	 cd keywords && awk '/^ -- Global/{print $$4}' global > global2 && rm global && mv global2 global

	awk '/^ -- Property/' keywords/keywords.txt > keywords/properties
	 cd keywords && awk '/^ -- Property/{print $$3}' properties > properties2 && rm properties && mv properties2 properties

	awk '/^ -- Special/' keywords/keywords.txt > keywords/special
	 cd keywords && awk '/^ -- Special/{print $$4}' special > special2 && rm special && mv special2 special

compile:
	${EMACS} --batch  -l test/make-install.el -L . -f batch-byte-compile maxima.el maxima-*.el

compile-test: compile clean
