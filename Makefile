include $(TOPDIR)/rules.mk

PKG_NAME:=racket
PKG_VERSION:=5.1
PKG_RELEASE=$(PKG_SOURCE_VERSION)

PKG_SOURCE_PROTO:=git
# PKG_SOURCE_URL:=git://github.com/tonyg/racket.git
PKG_SOURCE_URL:=/mnt/openwrt/racket
PKG_SOURCE_SUBDIR:=$(PKG_NAME)-$(PKG_VERSION)
PKG_SOURCE_VERSION:=openwrt
PKG_SOURCE:=$(PKG_NAME)-$(PKG_VERSION)-$(PKG_SOURCE_VERSION).tar.gz

PKG_INSTALL:=1

HOST_PATCH_DIR := ./patches-host

include $(INCLUDE_DIR)/package.mk
include $(INCLUDE_DIR)/host-build.mk

# Racket likes to be built in a separate VPATH, and in any case the
# src directory is where the configure script is, so we need to tell
# the build system to change to a subdirectory before
# configuring/building, and, having done so, to use ../configure
# instead of the default ./configure.
#
MAKE_PATH:=src/build
CONFIGURE_PATH:=$(MAKE_PATH)
CONFIGURE_CMD:=../configure

HOST_CONFIGURE_CMD:=../configure

# According to the Debian packaging of Racket, the MIPS architecture
# doesn't like the 3m collector, so we fall back to the Boehm
# collector here. TODO: revisit this once we have the MIPS machine
# available.
CONFIGURE_ARGS += \
	--enable-cgcdefault \
	--disable-plot \

# For simply building the .zos on the host, we don't need plot.
HOST_CONFIGURE_ARGS += \
	--disable-plot

# Another OpenWRT oversight: HOST_CPPFLAGS doesn't exist; instead, it
# uses HOST_CFLAGS. This wouldn't ordinarily be a problem, except that
# the -O2 that is included causes cpp to expand wchar.h differently,
# expanding out inline definitions of optimized versions of such as
# wctob(), which again ordinarily wouldn't be a problem except that
# xform.rkt doesn't quite get the lexing or printing of wide character
# literals (e.g. "L'\x7f'") right, and generates output such as
# "L '\x7f'", which isn't valid. So here we hack on things a bit to
# simulate HOST_CPPFLAGS, which should match HOST_CFLAGS except not
# include any -O2 flag.
#
# TODO: fix this so it's less brittle; -O3 shouldn't be included
# either, if HOST_CFLAGS ever changes to that!
# TODO: perhaps alter xform.rkt?
HOST_CPPFLAGS:=$(subst -O2,,$(HOST_CFLAGS))

# This should exactly match the definition in OpenWRT's host-build.mk,
# but for the change to using HOST_CPPFLAGS. See the comment above.
HOST_CONFIGURE_VARS = \
	CFLAGS="$(HOST_CFLAGS)" \
	CPPFLAGS="$(HOST_CPPFLAGS)" \
	LDFLAGS="$(HOST_LDFLAGS)" \
	SHELL="$(BASH)"

# 2011-05-16
# 16:35 < bremner_> right. well, I currently build debian packages this way
#                   (PLT_SETUP_OPTIONS="--no-zo --no-docs") most architectures.
PLT_SETUP_OPTIONS=--no-zo --no-docs

MAKE_INSTALL_FLAGS += PLT_SETUP_OPTIONS="$(PLT_SETUP_OPTIONS)"

# We need to tell the Boehm GC it is being cross-compiled.
MAKE_FLAGS += HOSTCC=$(CC) HOSTCFLAGS="-I$(PKG_BUILD_DIR)/src/racket/gc/include"

define Host/Configure
	mkdir -p $(HOST_BUILD_DIR)/$(CONFIGURE_PATH)
# BUG in OpenWRT: The Host/Configure/Default rule looks for
# ./configure, hard-coded, so setting HOST_CONFIGURE_CMD doesn't work
# as expected. Worked around by symlinking the file into the working
# directory, to make the test -x check pass, and then using
# ../configure to actually run it, in order for the relative path
# magic in ./configure to work out.
	ln -s $(HOST_BUILD_DIR)/$(CONFIGURE_PATH)/../configure $(HOST_BUILD_DIR)/$(CONFIGURE_PATH)
$(call Host/Configure/Default,,,$(CONFIGURE_PATH))
endef

define Host/Compile
# Disappointingly, OpenWRT's Host/Compile/Default rule doesn't let you
# override HOST_BUILD_DIR locally, so this is a straight copy of the
# text from that rule, modified as needed:
	$(MAKE) $(HOST_JOBS) -C $(HOST_BUILD_DIR)/$(CONFIGURE_PATH) $(1)
endef

define Host/Install
# Disappointingly, OpenWRT's Host/Install/Default rule doesn't let you
# override HOST_BUILD_DIR locally, so this is a straight copy of the
# text from that rule, modified as needed. We also add
# PLT_SETUP_OPTIONS here.
	$(_SINGLE)$(MAKE) -C $(HOST_BUILD_DIR)/$(CONFIGURE_PATH) \
		PLT_SETUP_OPTIONS="$(PLT_SETUP_OPTIONS)" \
		install
endef

define Build/Configure
	mkdir -p $(PKG_BUILD_DIR)/$(CONFIGURE_PATH)
$(call Build/Configure/Default)
endef

define Package/racket/Default
	SUBMENU:=Racket
	SECTION:=lang
	CATEGORY:=Languages
	TITLE:=Racket programming language
	URL:=http://www.racket-lang.org/
endef

define Package/racket/Default/description
	Racket is a programming language.
endef

define Package/racket-common
$(call Package/racket/Default)
	TITLE+= (libraries)
endef

define Package/racket-common/description
$(call Package/racket/Default/description)
	This package contains the compiled Racket libraries.
endef

define Package/racket
$(call Package/racket/Default)
	TITLE+= (interpreter)
endef

define Package/racket/description
$(call Package/racket/Default/description)
	This package contains the Racket interpreter.
endef

define Package/racket-common/install
	$(INSTALL_DIR) $(1)/usr/include
	$(CP) $(PKG_INSTALL_DIR)/usr/include/. $(1)/usr/include/
	$(INSTALL_DIR) $(1)/usr/share/racket/collects
	$(CP) $(PKG_INSTALL_DIR)/usr/share/racket/collects $(1)/usr/share/racket/collects
endef

define Package/racket/install
	$(INSTALL_DIR) $(1)/usr/bin
	$(INSTALL_BIN) $(PKG_INSTALL_DIR)/usr/bin/. $(1)/usr/bin/
	$(INSTALL_DIR) $(1)/usr/lib
	$(CP) $(PKG_INSTALL_DIR)/usr/lib/. $(1)/usr/lib/
endef

compile: host-install

$(eval $(call HostBuild))
$(eval $(call BuildPackage,racket-common))
$(eval $(call BuildPackage,racket))
