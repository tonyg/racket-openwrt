include $(TOPDIR)/rules.mk

# 2011-05-16
# 16:35 < bremner_> right. well, I currently build debian packages this way 
#                   (PLT_SETUP_OPTIONS="--no-zo --no-docs") most architectures.

PKG_NAME:=racket
PKG_VERSION:=5.1
PKG_RELEASE=$(PKG_SOURCE_VERSION)

PKG_SOURCE_PROTO:=git
# PKG_SOURCE_URL:=git://github.com/tonyg/racket.git
PKG_SOURCE_URL:=/mnt/openwrt/racket
PKG_SOURCE_SUBDIR:=$(PKG_NAME)-$(PKG_VERSION)
PKG_SOURCE_VERSION:=openwrt
PKG_SOURCE:=$(PKG_NAME)-$(PKG_VERSION)-$(PKG_SOURCE_VERSION).tar.gz

PKG_FIXUP:=autoreconf

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

# According to the Debian packaging of Racket, the MIPS architecture
# doesn't like the 3m collector, so we fall back to the Boehm
# collector here. TODO: revisit this once we have the MIPS machine
# available.
CONFIGURE_ARGS += \
	--enable-cgcdefault

# We need to tell the Boehm GC it is being cross-compiled.
MAKE_FLAGS += HOSTCC=$(CC) HOSTCFLAGS="-I$(PKG_BUILD_DIR)/src/racket/gc/include"

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
	$(CP) $(PKG_INSTALL_DIR)/usr/include/* $(1)/usr/include
	$(INSTALL_DIR) $(1)/usr/share/racket/collects
	$(CP) $(PKG_INSTALL_DIR)/usr/share/racket/collects $(1)/usr/share/racket/collects
endef

define Package/racket/install
	$(INSTALL_DIR) $(1)/usr/bin
	$(INSTALL_BIN) $(PKG_INSTALL_DIR)/usr/bin/* $(1)/usr/bin/
	$(INSTALL_DIR) $(1)/usr/lib
	$(CP) $(PKG_INSTALL_DIR)/usr/lib/* $(1)/usr/lib
endef

$(eval $(call BuildPackage,racket-common))
$(eval $(call BuildPackage,racket))
$(eval $(call HostBuild))
