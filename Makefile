version=$(shell jq -r '.version' elm-package.json)

all: build

dependencies:
	elm-github-install

build: dependencies
	elm-make src/Main.elm --output=assets/main.js

optimize: build
	closure-compiler --js_output_file=assets/main.min.js assets/main.js || true
	@original_size=$$(wc -c assets/main.js | cut -d' ' -f1); \
	new_size=$$(wc -c assets/main.min.js | cut -d' ' -f1); \
	echo Optimized file takes $$((100 * $$new_size / $$original_size)) % of the original size.
	mv assets/main.min.js assets/main.js

package: build_dir := $(shell mktemp -d)
package: optimize
	mkdir --parents "${build_dir}/almafoss-${version}"
	cp -r assets "${build_dir}/almafoss-${version}"
	cp index.html LICENSE README.md "${build_dir}/almafoss-${version}"
	tar czf "almafoss-${version}.tar.gz" -C "${build_dir}" "almafoss-${version}"
	rm -rf "${build_dir}"

watch: dependencies
	elm-live src/Main.elm --output=assets/main.js --pushstate --open --debug

