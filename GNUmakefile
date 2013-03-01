# -------------------------------------------------------------------------

# This is the development Makefile. It is used for compiling
# development versions and for creating the distributed package.
# This Makefile is not distributed.

SHELL := bash

.PHONY: test bootstrap all package check export godi clean

# -------------------------------------------------------------------------

# Compiling.

all:
	$(MAKE) $(MFLAGS) -C src -f GNUmakefile

# -------------------------------------------------------------------------

# Bootstraping.

bootstrap:
	$(MAKE) $(MFLAGS) -C src -f GNUmakefile bootstrap

# -------------------------------------------------------------------------

# Testing.

test:
	$(MAKE) -C bench

# -------------------------------------------------------------------------

# Cleaning up.

clean:
	@ for i in bench demos src ; do \
	  $(MAKE) $(MFLAGS) -C $$i $@ ; \
	done
	$(MAKE) $(MFLAGS) -rs -C doc $@

# -------------------------------------------------------------------------

# Prevent the built-in bash cd from displaying information.

export CDPATH=

# Distribution.
# The version number is automatically set to the current date.

DATE     := $(shell /bin/date +%Y%m%d)
PACKAGE  := menhir-$(DATE)
CURRENT  := $(shell pwd)
TARBALL  := $(CURRENT)/$(PACKAGE).tar.gz
HEADACHE := headache
SRCHEAD  := $(CURRENT)/header
LIBHEAD  := $(CURRENT)/lgpl-header

# GODI settings. We assume $(GODI_HOME) is defined and points
# to the host machine's GODI installation.

GODINAME := godi/godi-menhir
GODIWORK := /home/fpottier/dev/godi-build
GODISVN  := $(GODIWORK)/trunk/$(GODINAME)
GODIH    := $(GODI_HOME)/build/$(GODINAME)
GODIPACK := $(GODIWORK)/pack
GODIMAP  := $(GODIPACK)/release.4.00.map
GODIURL  := https://godirepo.camlcity.org/godi_admin
GODIVA   := $(GODI_HOME)/bin/godiva

# A list of files to copy without changes to the package.
#
# This does not include the src/ and doc/ directories, which require
# special treatment.

DISTRIBUTED_FILES := AUTHORS CHANGES INSTALLATION LICENSE Makefile Makefile.arch demos

# Some source files carry the "library" license, while others carry
# the regular "source code" license.

LIBFILES := \
  src/standard.mly \
  src/infiniteArray.{ml,mli} \
  src/packedIntArray.{ml,mli} \
  src/rowDisplacement.{ml,mli} \
  src/engineTypes.ml \
  src/engine.{ml,mli} \
  src/tableFormat.ml \
  src/tableInterpreter.{ml,mli} \
  src/convert.{ml,mli}

package: clean
# Create a directory to store the distributed files temporarily.
	@ rm -fr $(PACKAGE)
	@ mkdir -p $(PACKAGE)/src
	@ cp -fr $(DISTRIBUTED_FILES) $(PACKAGE)
	@ cp -fr src/*.ml{,i,y,l} src/Makefile src/META $(PACKAGE)/src
	@ $(MAKE) -C $(PACKAGE)/demos clean
# Insert headers.
	@ echo "-> Inserting headers."
	@ cd $(PACKAGE) && find . -regex ".*\.ml\(i\|y\|l\)?" \
	    -exec $(HEADACHE) -h $(SRCHEAD) "{}" ";"
	@ cd $(PACKAGE) && for file in $(LIBFILES) ; do \
	    $(HEADACHE) -h $(LIBHEAD) $$file ; \
	  done
# Set the version number into the files that mention it. These
# include version.ml, version.tex, META.
	@ echo "-> Setting version to $(DATE)."
	@ echo let version = \"$(DATE)\" > $(PACKAGE)/src/version.ml
	@ echo version = \"$(DATE)\" >> $(PACKAGE)/src/META
# Copy and compile the documentation.
	@ echo "-> Generating the documentation."
	@ cp -r doc $(PACKAGE)
	@ echo '\gdef\menhirversion{$(DATE)}' > $(PACKAGE)/doc/version.tex
	@ make -rs -C $(PACKAGE)/doc clean pdf
	@ mv $(PACKAGE)/doc/main.pdf $(PACKAGE)/manual.pdf
	@ mv $(PACKAGE)/doc/menhir.1 $(PACKAGE)/
	@ rm -rf $(PACKAGE)/doc
# Create the API documentation.
	@ $(MAKE) -C src api
# Create the tarball.
	@ echo "-> Tarball creation."
	tar --exclude=.svn -cvz -f $(TARBALL) $(PACKAGE)
	@ echo "-> Package $(PACKAGE).tar.gz is ready."

check:
	@ echo "-> Checking the package ..."
# Create a temporary directory; extract, build, and install the
# package into it; run the test suite using the installed binary.
	@ TEMPDIR=`mktemp -d /tmp/menhir-test.XXXXXX` && { \
	echo "   * Extracting. " && \
	(cd $$TEMPDIR && tar xfz $(TARBALL)) && \
	echo "   * Compiling and installing." && \
	mkdir $$TEMPDIR/install && \
	(cd $(PACKAGE) \
		&& make PREFIX=$$TEMPDIR/install USE_OCAMLFIND=false all install \
	) > $$TEMPDIR/install.log 2>&1 \
		|| (cat $$TEMPDIR/install.log; exit 1) && \
	echo "   * Running the test suite." && \
	$(MAKE) MENHIR=$$TEMPDIR/install/bin/menhir test > $$TEMPDIR/test.log 2>&1 \
		|| (cat $$TEMPDIR/test.log; exit 1) && \
	rm -fr $$TEMPDIR ; }
	@ echo "-> Package $(PACKAGE) seems ready for distribution!"

# -------------------------------------------------------------------------

# Copying to my Web site.

RSYNC   := scp -p -B -C
TARGET  := yquem.inria.fr:public_html/menhir/
PAGE    := /home/fpottier/dev/page

export:
# Copier l'archive et la doc vers yquem.
	$(RSYNC) $(TARBALL) $(TARGET)
	$(RSYNC) $(PACKAGE)/manual.pdf $(TARGET)
	$(RSYNC) CHANGES $(TARGET)
# Copier l'API vers la page Web.
	cd src && $(RSYNC) convert.mli.html $(TARGET)
# Mettre � jour la page Web de Menhir avec le nouveau num�ro de version.
	cd $(PAGE) && \
	  sed --in-place=.bak "s/menhir-[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]/$(PACKAGE)/" menhir.xml && \
	  $(MAKE) export && \
	  cvs commit -m "Updated Menhir's version number."

# -------------------------------------------------------------------------

# Creating a GODI package.

# This entry assumes that "make package" and "make export" have been
# run on the same day. It must have sufficient rights to write into
# the local GODI hierarchy.

godi:
	@ if [ `whoami` != "root" ] ; then \
	  echo "make godi must be run with root privileges. Try running ./godi" ; \
	  exit 1 ; \
	fi
	@ sed -e s/VERSION/$(DATE)/ < spec.godiva > $(GODISVN)/spec.godiva
	@ cd $(GODIWORK) && svn up
	@ cd $(GODISVN) && \
          $(GODIVA) -refetch -localbase $(GODI_HOME) spec.godiva && \
	  rsync -v -r $(GODIH)/ $(GODISVN) && \
	  chown -R fpottier $(GODISVN)
	@ echo "Do you wish to proceed and commit changes to GODI (yes or no)?"
	@ read answer && if [ "$$answer" != "yes" ] ; then \
	  echo Aborting. ; \
	  exit 1 ; \
	fi
	@ cd $(GODISVN) && \
	  svn commit -m "Changes to Menhir package." && \
	  export revision=`svn info Makefile | sed -n 's/^R.vision \?: \([0-9]\+\)$$/\1/p'` && \
	  echo Revision is now $$revision. && \
	  for map in $(GODIMAP) ; do \
	    sed --in-place=.bak "s/^\(.*menhir[^0-9]*\)[0-9]\+$$/\1$$revision/" $$map ; \
	  done
	@ echo Here are my changes to the release maps:
	@ cd $(GODIPACK) && svn diff --diff-cmd diff -x "-C0"
	@ echo "Do you wish to proceed and commit changes to GODI (yes or no)?"
	@ read answer && if [ "$$answer" != "yes" ] ; then \
	  echo Aborting. ; \
	  exit 1 ; \
	fi
	@ cd $(GODIPACK) && svn commit -m "Updated release map for Menhir."
	@ echo "You may now open GODI's release tool at"
	@ echo "        $(GODIURL)"
	@ echo "and proceed as directed. I will try to open this URL for you."
	@ case $$OSTYPE in \
	(linux*) \
	  su fpottier -c "firefox $(GODIURL)" ;; \
	(*) \
	  open $(GODIURL) ;; \
	esac
