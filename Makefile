
site: _site

_site: site.hs
	chcp 65001
	runghc site.hs rebuild

sync: _site
	s3cmd -P --delete-removed sync _site/ s3://www.timphilipwilliams.com/

clean:
	rm -r _site
	rm -r _cache
