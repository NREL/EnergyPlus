rmdir /s sdk-release\languages
rmdir /s sdk-release\examples
rmdir /s sdk-release\doc

mkdir sdk-release\languages\matlab
mkdir "sdk-release\languages\matlab\+SSC"
mkdir "sdk-release\languages\matlab\+SSC\@API"
mkdir "sdk-release\languages\matlab\+SSC\@Data"
mkdir "sdk-release\languages\matlab\+SSC\@Entry"
mkdir "sdk-release\languages\matlab\+SSC\@Info"
mkdir "sdk-release\languages\matlab\+SSC\@Module"
copy apiwrappers\matlab\ReadMe.txt sdk-release\languages\matlab
copy apiwrappers\matlab\modules.m sdk-release\languages\matlab
copy apiwrappers\matlab\pvwatts.m sdk-release\languages\matlab
copy apiwrappers\matlab\UIExample.m sdk-release\languages\matlab
copy apiwrappers\matlab\UIExample.fig sdk-release\languages\matlab
copy "apiwrappers\matlab\+SSC\ssccall.m" "sdk-release\languages\matlab\+SSC"
copy "apiwrappers\matlab\+SSC\@API\API.m" "sdk-release\languages\matlab\+SSC\@API"
copy "apiwrappers\matlab\+SSC\@Data\Data.m" "sdk-release\languages\matlab\+SSC\@Data"
copy "apiwrappers\matlab\+SSC\@Entry\Entry.m" "sdk-release\languages\matlab\+SSC\@Entry"
copy "apiwrappers\matlab\+SSC\@Info\Info.m" "sdk-release\languages\matlab\+SSC\@Info"
copy "apiwrappers\matlab\+SSC\@Module\Module.m" "sdk-release\languages\matlab\+SSC\@Module"

mkdir sdk-release\languages\python
copy apiwrappers\python\sscapi.py sdk-release\languages\python
rem copy apiwrappers\python\sscapi3.4.2.py sdk-release\languages\python
rem copy apiwrappers\python\openei_utilityrate_billcalculator.py sdk-release\languages\python

mkdir sdk-release\languages\php
copy apiwrappers\php\down-arrow.png sdk-release\languages\php
copy apiwrappers\php\jquery.searchabledropdown-1.0.8.min.js sdk-release\languages\php
copy apiwrappers\php\Makefile sdk-release\languages\php
copy apiwrappers\php\pvwatts.php sdk-release\languages\php
copy apiwrappers\php\sscphp.c sdk-release\languages\php

mkdir sdk-release\languages\csharp
mkdir sdk-release\languages\csharp\Properties
copy apiwrappers\csharp\SSC.cs sdk-release\languages\csharp
copy apiwrappers\csharp\App.config sdk-release\languages\csharp
copy apiwrappers\csharp\Program.cs sdk-release\languages\csharp
copy apiwrappers\csharp\TestSSCAPI.csproj sdk-release\languages\csharp
copy apiwrappers\csharp\TestSSCAPI.sln sdk-release\languages\csharp
copy apiwrappers\csharp\TestSSCAPIForm.cs sdk-release\languages\csharp
copy apiwrappers\csharp\TestSSCAPIForm.Designer.cs sdk-release\languages\csharp
copy apiwrappers\csharp\TestSSCAPIForm.resx sdk-release\languages\csharp
copy apiwrappers\csharp\Properties\AssemblyInfo.cs sdk-release\languages\csharp\Properties
copy apiwrappers\csharp\Properties\Resources.Designer.cs sdk-release\languages\csharp\Properties
copy apiwrappers\csharp\Properties\Resources.resx sdk-release\languages\csharp\Properties

mkdir sdk-release\languages\java
mkdir sdk-release\languages\java\SSC
copy apiwrappers\java\TestBelpe.data sdk-release\languages\java
copy apiwrappers\java\TestBelpe.java sdk-release\languages\java
copy apiwrappers\java\TestBelpe.make sdk-release\languages\java
copy apiwrappers\java\TestBelpe.Manifest.txt sdk-release\languages\java
rem copy apiwrappers\java\TestCashLoan.data sdk-release\languages\java
rem copy apiwrappers\java\TestCashLoan.java sdk-release\languages\java
rem copy apiwrappers\java\TestCashLoan.make sdk-release\languages\java
rem copy apiwrappers\java\TestCashLoan.Manifest.txt sdk-release\languages\java
copy apiwrappers\java\TestPVSamV1.java sdk-release\languages\java
copy apiwrappers\java\TestPVSamV1.make sdk-release\languages\java
copy apiwrappers\java\TestPVSamV1.Manifest.txt sdk-release\languages\java
rem copy apiwrappers\java\TestUtilityRate3.data sdk-release\languages\java
rem copy apiwrappers\java\TestUtilityRate3.java sdk-release\languages\java
rem copy apiwrappers\java\TestUtilityRate3.make sdk-release\languages\java
rem copy apiwrappers\java\TestUtilityRate3.Manifest.txt sdk-release\languages\java
copy apiwrappers\java\ReadMe.txt sdk-release\languages\java
copy apiwrappers\java\sscapi_wrap.c sdk-release\languages\java
copy apiwrappers\java\TestSSCAPI.java sdk-release\languages\java
copy apiwrappers\java\TestSSCAPI.make sdk-release\languages\java
copy apiwrappers\java\TestSSCAPI.Manifest.txt sdk-release\languages\java
copy apiwrappers\java\SSC\API.java sdk-release\languages\java\SSC
copy apiwrappers\java\SSC\Data.java sdk-release\languages\java\SSC
copy apiwrappers\java\SSC\Entry.java sdk-release\languages\java\SSC
copy apiwrappers\java\SSC\Info.java sdk-release\languages\java\SSC
copy apiwrappers\java\SSC\Module.java sdk-release\languages\java\SSC
copy apiwrappers\java\SSC\SSCAPIJNI.java sdk-release\languages\java\SSC

mkdir sdk-release\examples
USA AZ Phoenix (TMY2)
copy "examples\USA AZ Phoenix (TMY2).csv" sdk-release\examples
copy examples\abilene.tm2 sdk-release\examples
copy examples\coeffgen_example.c sdk-release\examples
copy examples\example1_pvwatts.c sdk-release\examples
copy examples\pvwatts1ts_ex.c sdk-release\examples
copy examples\pvwattsv5run.c sdk-release\examples
copy examples\ver.c sdk-release\examples

copy ..\lk\doc\lk_guide.pdf sdk-release\
copy ssc\sscapi.h sdk-release\
copy doc\ssc_guide.pdf sdk-release\
