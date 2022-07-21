include $(top_srcdir)/build/modmake.rulesam

MODNAME=mod_asinfo_tts

mod_LTLIBRARIES = mod_asinfo_tts.la
mod_asinfo_tts_la_SOURCES  = mod_asinfo_tts.c

mod_asinfo_tts_la_CFLAGS   = $(AM_CFLAGS)
mod_asinfo_tts_la_CFLAGS  += -I.

mod_asinfo_tts_la_LIBADD   = $(switch_builddir)/libfreeswitch.la
mod_asinfo_tts_la_LDFLAGS  = -avoid-version -module -no-undefined -shared

