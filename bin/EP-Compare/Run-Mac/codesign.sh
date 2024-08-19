#!/bin/bash

APP_NAME=EP-Compare
IDENTIFIER="org.nrel.EnergyPlus.$APP_NAME"

function ep_codesign() {
  codesign -vvvv -s "Developer ID Application: National Renewable Energy Laboratory (K7JYVQJL7R)" \
    --force --timestamp \
    --prefix "$IDENTIFIER." \
    --options runtime \
    --entitlements entitlements.xml "$1"
}

function ep_notarize() {
  xcrun notarytool submit --keychain-profile "EnergyPlus" --wait "$1"
}

ep_codesign "EP-Compare.app/Contents/Frameworks/RBAppearancePak.rbx_0.dylib"
ep_codesign "EP-Compare.app/Contents/Frameworks/TreeView.rbx_0.dylib"
ep_codesign "EP-Compare.app/Contents/Frameworks/#CoreClasses.rbx_5.dylib"
ep_codesign "EP-Compare.app/Contents/Frameworks/#CoreClasses.rbx_1.dylib"
ep_codesign "EP-Compare.app/Contents/Frameworks/#CoreClasses.rbx_0.dylib"
ep_codesign "EP-Compare.app/Contents/Frameworks/MBS REALbasic ChartDirector Plugin.rbx_0.dylib"

ep_codesign "EP-Compare.app/Contents/MacOS/rbframework.dylib"
ep_codesign "EP-Compare.app/Contents/MacOS/EP-Compare"

codesign -vvvv -s "Developer ID Application: National Renewable Energy Laboratory (K7JYVQJL7R)" \
    --force --timestamp \
    --identifier "$IDENTIFIER" \
    --options runtime \
    --entitlements entitlements.xml \
    $APP_NAME.app

zip -r ./$APP_NAME.zip ./$APP_NAME.app
ep_notarize ./$APP_NAME.zip
xcrun stapler staple ./$APP_NAME.app

xcrun stapler validate ./$APP_NAME.app
spctl -vvvv --assess ./$APP_NAME.app
rm -Rf ./$APP_NAME.zip
