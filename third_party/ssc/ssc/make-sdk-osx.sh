cp -v build_osx/ssc.dylib sdk-release/osx64/ssc.dylib
mkdir -p sdk-release/osx64/SDKtool.app
cp -v -R build_osx/SDKtool.app/* sdk-release/osx64/SDKtool.app
strip sdk-release/osx64/ssc.dylib
strip sdk-release/osx64/SDKtool.app/Contents/MacOS/SDKtool
