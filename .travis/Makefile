MAKEFLAGS += --warn-undefined-variables -j1
SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
.DEFAULT_GOAL := all
.DELETE_ON_ERROR:
.SUFFIXES:
.PHONY: package

# Environment switches
MODULE ?= ejabberd-read-markers
VERSION ?= 18.01
MOD_VERSION ?= latest
SOURCE_URL ?= https://github.com/processone/ejabberd/archive/$(VERSION).tar.gz

# Directories
INCLUDE_DIRS ?= ../include
EBIN_DIRS ?= ../ebin
SRC_DIR ?= ../src

# Host binaries
APTGET ?= apt-get
CD ?= cd
CP ?= cp
ERLC ?= erlc
FIND ?= find
MKDIR ?= mkdir
PWD ?= pwd
SED ?= sed
SUDO ?= sudo
TAR ?= tar
TEST ?= test
WGET ?= wget

.download-ejabberd-sources:
	# Download the ejabberd $(VERSION) sources
	@$(TEST) -f ejabberd/autogen.sh || ( \
		$(WGET) -O ejabberd.tar.gz $(SOURCE_URL) && \
		$(MKDIR) -p ejabberd && \
		$(TAR) xf ejabberd.tar.gz -C ejabberd --strip-components=1 \
	)

.install-ejabberd-build-deps:
	# Install the ejabberd $(VERSION) build dependencies
	@$(SUDO) $(APTGET) update -y
	@$(SUDO) $(APTGET) build-dep -y ejabberd
	@$(SUDO) $(APTGET) install -y libssl-dev libyaml-dev libgd-dev libwebp-dev

.build-ejabberd:
	# Build ejabberd $(VERSION) from source
	@cd ejabberd && ./autogen.sh && ./configure && $(MAKE)

.find-deps:
	# Find all build dependencies
	$(eval INCLUDES = $(addprefix -I ,\
		$(shell $(FIND) `$(PWD)` -type d -name include) $(INCLUDE_DIRS)))
	$(eval EBINS = $(addprefix -pa ,\
		$(shell $(FIND) `$(PWD)` -type d -name ebin) $(EBIN_DIRS)))

install: \
	.download-ejabberd-sources \
	.install-ejabberd-build-deps \
	.build-ejabberd \
	.find-deps

build: install
	# Build $(MODULE) module from source
	@$(MKDIR) -p $(EBIN_DIRS)
	@$(ERLC) \
		-o $(EBIN_DIRS) \
		$(INCLUDES) \
		$(EBINS) \
		-DLAGER \
		-DNO_EXT_LIB \
		$(SRC_DIR)/*.erl

package:
	# Create a new release package ($(MODULE)-$(MOD_VERSION).tar.gz)
	@$(MKDIR) -p package package/{conf,sql}
	@$(CP) -r $(EBIN_DIRS) package/
	@$(CP) ../LICENSE ../README.md ../INSTALL.md \
		../mod_read_markers.spec ../CHANGELOG.md \
		package/
	@$(CP) ../config/mod_read_markers.yml package/conf/
	@$(CP) ../config/postgres/99-pg-read-markers.sql package/sql/pg.sql
	@$(SED) -i -e '/\\connect/d' -e '/^$$/d' package/sql/pg.sql
	@$(CD) package && \
		$(TAR) cfvz ../$(MODULE)-$(MOD_VERSION).tar.gz --owner=0 --group=0 .
	@$(RM) -rf package
