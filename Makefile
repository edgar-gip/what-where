# what-where --- Find what you look at and where you are.
#
# Copyright (C) 2017  Edgar Gonzàlez i Pellicer
#
# Author: Edgar Gonzàlez i Pellicer <edgar.gip@gmail.com>
# Keywords: what, where
# Version: 0.1
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <http://www.gnu.org/licenses/>.

PACKAGE = what-where
VERSION = 0.1

SOURCES      = $(wildcard *.el)
DEEP_SOURCES = $(wildcard $(PACKAGE)/*.el)

.PHONY: all
all: $(PACKAGE)-$(VERSION).tar

$(PACKAGE)-$(VERSION).tar: $(SOURCES) $(DEEP_SOURCES)
	rm -rf $(PACKAGE)-$(VERSION)
	mkdir $(PACKAGE)-$(VERSION)
	cp -f $(SOURCES) $(PACKAGE)-$(VERSION)
	mkdir $(PACKAGE)-$(VERSION)/$(PACKAGE)
	cp -f $(DEEP_SOURCES) $(PACKAGE)-$(VERSION)
	tar cf $@ $(PACKAGE)-$(VERSION)
	rm -rf $(PACKAGE)-$(VERSION)

# Local Variables:
# coding: utf-8
# End:
