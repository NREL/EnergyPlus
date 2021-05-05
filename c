[33mcommit 2b1fbaf3a1dc9fae0c19d08cde4f1d343a083eb8[m[33m ([m[1;36mHEAD -> [m[1;32mdevelop[m[33m, [m[1;31morigin/develop[m[33m, [m[1;31morigin/HEAD[m[33m)[m
Author: khaddad <khaddad.energy.eng@gmail.com>
Date:   Tue Feb 18 12:19:17 2020 -0500

    design document for advanced BIPVT model

[33mcommit 2336dc564f908899fd1593ab4009f50fd8a793d4[m
Merge: 651b53da3 e93f1805b
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Fri Jan 17 19:31:26 2020 -0700

    Merge pull request #7684 from NREL/FixupPlantLoopEquipCalls
    
    Cleanup some lingering issues in PlantLoopEquip

[33mcommit 651b53da3ba0cc9718b09f3a343f6ab2186abdc3[m
Merge: cdf992fb3 b3230a484
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Fri Jan 17 13:56:14 2020 -0600

    Merge pull request #7635 from NREL/169953819_Issue7471
    
    Addresses Set DOAS Set point temp to default if fields are blank

[33mcommit cdf992fb3678fe5d4bfe54e4560ce2f8c5cc2e33[m
Merge: 700637e04 14aa63c76
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Fri Jan 17 12:27:33 2020 -0700

    Merge pull request #7685 from NREL/AbsorberChillerRefactor
    
    Plant component refactor of Absorption ChillerHeater

[33mcommit e93f1805bcdf8c8af05fcd090bcb319a28fbbc09[m
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Fri Jan 17 12:37:48 2020 -0600

    Clean up unused runflag setting stuff, it may be good now

[33mcommit 14aa63c761182899faa81034b248705e33b837c7[m
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Fri Jan 17 06:41:28 2020 -0600

    Fix extraneous size() call

[33mcommit 2629c64a317f40d46110a0acc9acc3f103cf4939[m
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Fri Jan 17 00:11:43 2020 -0600

    Apply style

[33mcommit d52617d1f0e739bae5bb36f7daa62cc7ade8e7df[m
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Fri Jan 17 00:10:27 2020 -0600

    Convert to plant component

[33mcommit da7be4c4b890ead14bc9b33821027e377756d86f[m
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Thu Jan 16 23:51:19 2020 -0600

    Fix compile issue, move setupOutputVar to method

[33mcommit 006d0c2ee7a2765afdde415d61424ca459ac6a02[m
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Thu Jan 16 23:28:46 2020 -0600

    Clean up some local variable issues and IDE warnings

[33mcommit 4e74deff56322425efbfd5eecc551ad24092dc8b[m
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Thu Jan 16 23:18:07 2020 -0600

    Cleaning up externs and namespace vars

[33mcommit f13e2e5d605cc31523a6d4b4a99db3962f482597[m
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Thu Jan 16 22:54:55 2020 -0600

    Remove using statements

[33mcommit df8b2c73a242ade8b2e7f2271312ae9d3d63cfe3[m
Merge: 396d3db37 700637e04
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Thu Jan 16 22:37:18 2020 -0600

    Merge remote-tracking branch 'origin/develop' into AbsorberChillerRefactor

[33mcommit 8c2b296df883fd029cfd87dfb36dcdaaeedae31c[m
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Thu Jan 16 21:01:16 2020 -0600

    Fix compiling issues

[33mcommit c83b907c074463fecbaf5aa7b6a2bd2b3e16f4e9[m
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Thu Jan 16 20:57:44 2020 -0600

    Clean up last of the remaining specialty things, now just a couple chillers and the refactor is done

[33mcommit c3d3ddded8105a8087c43db08b9a090ea576b641[m
Merge: 5113166f4 700637e04
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Thu Jan 16 20:45:29 2020 -0600

    Merge remote-tracking branch 'origin/develop' into FixupPlantLoopEquipCalls

[33mcommit 700637e042f87a327e93036f9a7ecd99647c35fb[m
Merge: cfd61961f bdd680021
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Thu Jan 16 20:43:59 2020 -0600

    Merge pull request #7679 from NREL/chillerElecEIRrefactor
    
    Chiller Electric EIR Plant Component Refactor

[33mcommit cfd61961f56a81d712ce2ac145dfd8f5de691544[m
Merge: ce98bde0b e80346495
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Thu Jan 16 20:41:30 2020 -0600

    Merge pull request #7689 from NREL/FixPackaging
    
    Remove 9-3 idd install from idd/CMakeLists.txt

[33mcommit 5113166f41fce90a5a611bd76ac5d2f98172adc2[m
Merge: 1d4fc5b9a ce98bde0b
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Thu Jan 16 15:10:14 2020 -0700

    Merge branch 'develop' into FixupPlantLoopEquipCalls

[33mcommit 1d4fc5b9a69a6427d388b8ecd5ec260dfcc7ba86[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Thu Jan 16 15:02:30 2020 -0700

    remove dynamic_cast<> from PlantLoopEquip

[33mcommit bdd6800210d2772a5a89b37fcb6349d2f6e8acf9[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Thu Jan 16 14:21:20 2020 -0700

    pull setting EquipFlowCtrl out of PlantLoopEquip

[33mcommit 2ff63bd6745d939fe1fb033eebb7f18b988115ee[m
Merge: adf7ba052 ce98bde0b
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Thu Jan 16 13:59:36 2020 -0700

    Merge branch 'develop' into chillerElecEIRrefactor

[33mcommit e803464959078ea69d494b0f692039a24120376d[m
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Thu Jan 16 14:30:16 2020 -0600

    Remove 9-3 idd install from idd/CMakeLists.txt
    
    It is a generated file, we don't install it from this folder.

[33mcommit b3230a4842f5781cc501e16c066efe30e0ac7e9f[m
Author: nigusse <bnigusse@fsec.ucfedu>
Date:   Thu Jan 16 13:50:47 2020 -0500

    reverted the change for checking blank fields

[33mcommit 4c6ee673cabef2f80e3d9c38853d21d116297c73[m
Merge: 0a2d2a330 ce98bde0b
Author: nigusse <bnigusse@fsec.ucfedu>
Date:   Thu Jan 16 13:07:51 2020 -0500

    Merge branch 'develop' into 169953819_Issue7471

[33mcommit 0a2d2a330265a403151f18e6559f0e7eb5be49ce[m
Author: nigusse <bnigusse@fsec.ucfedu>
Date:   Thu Jan 16 13:06:50 2020 -0500

    Sizing:Zone object Min-Field increased to 27 to cover all fields

[33mcommit ce98bde0b2a07ce421fc4abf5961244eb5f08cf0[m
Merge: b86c6f5e1 32d5892d5
Author: Richard Raustad <rraustad@fsec.ucf.edu>
Date:   Thu Jan 16 12:50:18 2020 -0500

    Merge pull request #7669 from NREL/hvacVRFRefactor
    
    ZoneHVAC VRF Plant Component Refactor

[33mcommit b86c6f5e120eaf7b1be89f4988c909d8ec34461f[m
Merge: 717bebf38 0eff4863d
Author: Michael J. Witte <mjwitte@gard.com>
Date:   Thu Jan 16 11:45:53 2020 -0600

    Merge pull request #7659 from NREL/FixMinFieldsAndIdfEd
    
    Add warning to IDF Editor when \min-fields is greater than number of fields

[33mcommit 717bebf383688d94503db39fd331ef80073c9685[m
Merge: a085e31d2 fc10cf8e0
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Thu Jan 16 10:51:15 2020 -0600

    Merge pull request #7677 from NREL/remove_objexx_gio__2
    
    Remove objexx gio part 2

[33mcommit adf7ba0524d6e368c536bbcef045a27cb4036b1a[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Thu Jan 16 09:26:16 2020 -0700

    apply style

[33mcommit 7863112711297a8c6eb36f9e62489a46bc152adf[m
Merge: 30e2c1184 a085e31d2
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Thu Jan 16 04:55:13 2020 -0700

    Merge branch 'develop' into chillerElecEIRrefactor

[33mcommit 32d5892d5e82b6d7ab50c5141cf2c5299c3387f6[m
Merge: 3b9b51326 a085e31d2
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Thu Jan 16 04:54:47 2020 -0700

    Merge branch 'develop' into hvacVRFRefactor

[33mcommit a085e31d2bd3b9abc566f01ac1fad586dca1de6b[m
Merge: 7ff93c0f8 4cb0da233
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Thu Jan 16 04:39:50 2020 -0700

    Merge pull request #7637 from NREL/ChillerPlantComponentAgain
    
    Chiller Plant Component Refactor

[33mcommit fc10cf8e0c503a9a0440d79191d67e2bc1e09ed7[m
Merge: fa4b8db02 aff110d6b
Author: Jason Turner <jason@emptycrate.com>
Date:   Wed Jan 15 18:06:51 2020 -0700

    Merge branch 'remove_objexx_gio__2' of github.com:NREL/EnergyPlus into remove_objexx_gio__2

[33mcommit fa4b8db024e493a3daaf282e3ef721fc473916bf[m
Merge: 77299b17e 7ff93c0f8
Author: Jason Turner <jason@emptycrate.com>
Date:   Wed Jan 15 18:06:23 2020 -0700

    Merge remote-tracking branch 'origin/develop' into remove_objexx_gio__2

[33mcommit 396d3db37b98697bfaf1bf2ecfcc09bab39b6618[m
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Wed Jan 15 18:26:38 2020 -0600

    Initial cleanup pass on gas absorption chillerheater

[33mcommit 43499416c51363654302accd1109c9ed75e36c62[m
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Wed Jan 15 17:46:44 2020 -0600

    Cleanup some lingering bandaids in plantloopequip

[33mcommit 3b9b51326b49f2c7c30394f1f9619b66527a3752[m
Merge: aa05edef7 7ff93c0f8
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 15:35:25 2020 -0700

    Merge branch 'develop' into hvacVRFRefactor

[33mcommit 7ff93c0f885e7955217a876c59b1bb82d1546d90[m
Merge: 80db32762 39cee889b
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Wed Jan 15 15:44:41 2020 -0600

    Merge pull request #7489 from NREL/EIR_AWHP
    
    Extend EIR WWHP to Air-source

[33mcommit 30e2c118491c23a1c3ccf3bc271f6408dd4af59e[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 14:17:32 2020 -0700

    add getDesignTemperatures from PlantComponent

[33mcommit 3896f13e5a70c66f91b323cb899417f1e970cc44[m
Author: nigusse <bnigusse@fsec.ucfedu>
Date:   Wed Jan 15 14:22:42 2020 -0500

    added one more unit test and updated format

[33mcommit 027063a54403d1d7dab55f1fe4637cd8a72df987[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 12:20:50 2020 -0700

    step 10 - move setupOutputVars to member function

[33mcommit 932a1763b206c883777b855f7866c306fbe8d886[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 12:09:41 2020 -0700

    steps 11-12 - inherit PlantComponent and move over to PlantComp calling structure

[33mcommit 533c5c3f97055ac07ba16d87b009f11844235a98[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 10:46:46 2020 -0700

    step 9 - functions to members and unit test cleanup

[33mcommit 17cb530337fa17651634a9d62104916aeeea3cd6[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 10:28:02 2020 -0700

    step 9 - partial functions to members

[33mcommit 0eff4863d5df0f9972dc81d7d790bc234b0c835b[m
Merge: 76953b267 80db32762
Author: mjwitte <mjwitte@gard.com>
Date:   Wed Jan 15 11:16:46 2020 -0600

    Merge remote-tracking branch 'remotes/origin/develop' into FixMinFieldsAndIdfEd

[33mcommit 3858c3437d77d63f45957835d93cfb9727d5a2df[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 10:16:00 2020 -0700

    step 9 - partial functions to members

[33mcommit ec730c058483f1f485fba40f6270d923ed3cf72e[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 10:01:03 2020 -0700

    step 9 - partial cleanup local vars

[33mcommit 39cee889b29894bd24ab4aeeeb16b30d1ff39932[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 09:39:06 2020 -0700

    fix transitions

[33mcommit 9b490def213fd7bdeb8effc2d5f3de2c75810796[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 09:32:36 2020 -0700

    step 9 - partial cleanup local vars

[33mcommit e7b545b1fd3b4541e97a7bf9242b53d4108e83f2[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 09:24:50 2020 -0700

    step 9 - partial cleanup local vars

[33mcommit 37870061f85b63001f59aadc7577a0d6c9607bfa[m
Merge: d4436736a 80db32762
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 08:53:38 2020 -0700

    Merge branch 'develop' into chillerElecEIRrefactor

[33mcommit a8fb2ff4f707bfe4c7172ea878f61adbe142d2a9[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 08:52:44 2020 -0700

    fix nodiff

[33mcommit 094c81180c67a6b12609f87ae3e3479e7d6d3b0b[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 08:46:08 2020 -0700

    add transition rules

[33mcommit 6c08174fb23d0f81920354fe76144eadca160a69[m
Merge: 1cc299ebb 80db32762
Author: nigusse <bnigusse@fsec.ucfedu>
Date:   Wed Jan 15 10:44:31 2020 -0500

    Merge branch 'develop' into 169953819_Issue7471

[33mcommit 76953b26720a192018c6d16e25aeaf372f7438b3[m
Author: JasonGlazer <jglazer@gard.com>
Date:   Wed Jan 15 09:36:04 2020 -0600

    Single warning if multiple \min-fields are a problem and fix typos

[33mcommit 9dee1faedd38eec9fe2d19d516f670f53c71e423[m
Merge: 6188ad6b9 80db32762
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 08:28:33 2020 -0700

    Merge branch 'develop' into EIR_AWHP

[33mcommit 80db32762b472b3449cf453b4775fbc87a17e4f4[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 08:16:34 2020 -0700

    fix missed V92->93 updates

[33mcommit aff110d6b03c63cc482e715d3670c86ac41a1693[m
Author: Jason Turner <jason@emptycrate.com>
Date:   Wed Jan 15 08:12:28 2020 -0700

    Patch `fmt` to avoid Objexx::fmt namespace collision
    
    @mjwitte I believe this is the correct solution instead of chasing down
    potential conflicts over and over.
    
    I've submitted a patch to {fmt} https://github.com/fmtlib/fmt/pull/1522

[33mcommit d4436736af75a035b3f1ddb762f451008b1f9cbf[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 05:24:16 2020 -0700

    step 9 - partial cleanup local vars

[33mcommit 6188ad6b9e35adabd9c9c0c8e3ae446944f2d282[m
Author: Edwin Lee <leeed2001@gmail.com>
Date:   Wed Jan 15 06:17:51 2020 -0600

    Fix min-fields

[33mcommit 4fb43e4288d9a0903f9d36805a0d9e34d5012b1e[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Wed Jan 15 05:13:07 2020 -0700

    step 6 - cleanup remaining mod-level vars

[33mcommit 31f58df6b11cbe0cb7c377dd67dfabc6b5976234[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Tue Jan 14 17:30:37 2020 -0700

    step 7 - remove extra structs

[33mcommit 1ff7f7f07fb69ffa58e688a66c058dab53751c33[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Tue Jan 14 17:22:55 2020 -0700

    step 7 - partial remove extra struct

[33mcommit 715b5b7c1f500e89b55fabb3dbadc434a670af76[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Tue Jan 14 17:07:09 2020 -0700

    fix diffs

[33mcommit 15d0544a1e124a15a8efa9c2f5bd5bc768aa1f22[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Tue Jan 14 13:29:55 2020 -0700

    steps 4-7 - partial cleanup/move/remove mod vars, remove extra structs

[33mcommit 2b8b8d007c43bb69aa453712134c543b112b26fc[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Tue Jan 14 13:15:32 2020 -0700

    steps 4-7 - partial cleanup/move/remove mod vars, remove extra structs

[33mcommit 79fc81a4743791080d4a1304c0a365db31fbf903[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Tue Jan 14 12:30:40 2020 -0700

    steps 4-7 - partial cleanup/move/remove mod vars, remove extra structs

[33mcommit ccbf421734a17eceee8e6a69b95c4ad58b13e8bc[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Tue Jan 14 12:10:31 2020 -0700

    steps 4-7 - partial cleanup/move/remove mod vars, remove extra structs

[33mcommit 4cba12bc4f4b31105c8b415e9aa992b7c0cbdafd[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Tue Jan 14 11:48:35 2020 -0700

    fix botched merge

[33mcommit 9601a797016303ca347b4e10870415b010110007[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Tue Jan 14 10:57:17 2020 -0700

    steps 4-7 - partial cleanup/move/remove mod vars, remove extra structs

[33mcommit 4f955df8b4a6e30d2bc1f79ea8e30722826e0b69[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Tue Jan 14 10:40:23 2020 -0700

    steps 4-7 - partial cleanup/move/remove mod vars, remove extra structs

[33mcommit 8217e4abeb3cbcaf965dc4dae09a7f5e5bc6158d[m
Merge: fbbbd66b9 6d979341f
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Tue Jan 14 10:00:16 2020 -0700

    Merge branch 'develop' into chillerElecEIRrefactor

[33mcommit aa05edef7520003f53216e144feea33d4c67f862[m
Merge: 134cec9d4 6d979341f
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Tue Jan 14 09:59:45 2020 -0700

    Merge branch 'develop' into hvacVRFRefactor

[33mcommit 87ee1e2e36c5a0ec0bf1d70d9871b9c14bef6ca3[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Tue Jan 14 09:56:20 2020 -0700

    apply style to unit test

[33mcommit 17a40370b24a1809414d52bf99df7cb4908d871c[m
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Tue Jan 14 09:54:25 2020 -0700

    fix merge issues

[33mcommit 4f9732ac1425e80c826c6edef7772b8423e3156d[m
Merge: 18ed72f82 6d979341f
Author: Matt Mitchell <matt.mitchell@nrel.gov>
Date:   Tue Jan 14 09:50:00 2020 -0700

    Merge branch 'develop' into EIR_AWHP
