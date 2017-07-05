#/bin/sh

./gradlew -Dxodus-from-maven=true  build
(unzip -o build/entity-browser-launcher.zip -d build/browser && cd build/browser && ./run.sh)
