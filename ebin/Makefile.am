## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

appdir = $(beamdir)
app_DATA = usekeeper.app
EXTRA_DIST = usekeeper.app

beam_DATA = usekeeper_app.beam \
			usekeeper_sup.beam \
			usekeeper_server.beam \
			usekeeper_rest.beam \
			usekeeper_rest_usage_specification.beam \
			mod_usekeeper_rest_accepted_content.beam \
			mod_usekeeper_rest_post.beam \
			mod_usekeeper_rest_get.beam \
			mod_usekeeper_rest_delete.beam \
			mod_usekeeper_rest_patch.beam \
			usekeeper_rest_pagination_server.beam \
			usekeeper_rest_pagination_sup.beam \
			usekeeper_rest_res_user.beam \
			usekeeper.beam

%.beam:$(top_srcdir)/src/%.erl	$(top_srcdir)/include/usage.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include -I $(top_builddir)/include $<

%.app:   %.app-in
	sed -e 's/%APP_NAME%/$(PACKAGE_NAME)/' \
			-e 's/%APP_VERSION%/$(PACKAGE_VERSION)/' \
			-e 's#%APP_PRIV%#$(ERLANG_INSTALL_LIB_DIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION)/priv#' < $< > $@

CLEANFILES = $(beam_DATA) $(app_DATA)

