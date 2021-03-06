(defun day-four-part-one (input)
  (apply #'+ (mapcar #'score-passphrase input)))

(defun valid-passphrase-p (phrase &key (test #'string=))
  (equal (remove-duplicates phrase :test test)
         phrase))

(defun score-passphrase (phrase &key (test #'string=))
  (if (valid-passphrase-p phrase :test test)
      1
      0))

(defparameter *input-test-one*
  (list
    (list "aa" "bb" "cc" "dd" "ee")
    (list "aa" "bb" "cc" "dd" "aa")
    (list "aa" "bb" "cc" "dd" "aaa")))

(assert (= (day-four-part-one *input-test-one*) 2))

(defun day-four-part-two (input)
  (apply #'+ (mapcar (lambda (phrase)
                       (score-passphrase phrase :test #'anagram-p))
                     input)))

(defun anagram-p (a b)
  (string= (sort a #'char<) (sort b #'char<)))

(defparameter *input-test-two*
  (list
    (list "abcde" "fghij")
    (list "abcde" "xyz" "ecdab")
    (list "a" "ab" "abc" "abd" "abf" "abj")
    (list "iiii" "oiii" "ooii" "oooi" "oooo")
    (list "oiii" "ioii" "iioi" "iiio")))

(assert (= (day-four-part-two *input-test-two*) 3))

(defparameter *input* 
  (list
    (list "pphsv" "ojtou" "brvhsj" "cer" "ntfhlra" "udeh" "ccgtyzc" "zoyzmh" "jum" "lugbnk")
    (list "vxjnf" "fzqitnj" "uyfck" "blnl" "impo" "kxoow" "nngd" "worcm" "bdesehw")
    (list "caibh" "nfuk" "kfnu" "llfdbz" "uxjty" "yxjut" "jcea")
    (list "qiho" "qif" "eupwww" "avyglnj" "nxzotsu" "hio" "lws")
    (list "xjty" "usocjsh" "pivk" "qnknunc" "yjcgh" "bwya" "djw" "zpyr")
    (list "ycfmfe" "mgq" "sjiomg" "nfzjul" "bjwkmgu" "yvsnvgj" "dcjupu" "wzz" "blmn")
    (list "rdowgbt" "vpwfdoi" "blzl" "laghnk" "gsa" "vhnpo" "cztxzlb" "rtz" "hvwonhb" "eciju" "pfjtbo")
    (list "bqs" "bqs" "dbutvgf" "mmzb" "izpyud" "rap" "izpyud" "xlzeb" "mnj" "hjncs")
    (list "xpu" "vwp" "nujcos" "piu" "irindir" "tpmfd" "umtvlm" "gznu")
    (list "sfpuxar" "qcnbte" "omouazv" "cnh" "uaxspfr" "sepolf" "rusafpx")
    (list "xbmaf" "iceyqqq" "sabpt" "gliexel" "muubepe" "qqiyqce" "fmrcc" "eazk" "obkeonl" "fmccr" "kgk")
    (list "apg" "gbycwe" "gap" "pag")
    (list "gagv" "saqbk" "lwtllc" "wnhzz" "khxsjc")
    (list "lgc" "alen" "rlmsp" "anel" "gcbvg")
    (list "bujlaz" "rks" "rlqf" "deknmee" "yrp")
    (list "scqvl" "weusbc" "bgvaz" "vgg" "cjwsfno" "vqy" "zbq" "aqy" "tvf" "bgzav")
    (list "hbki" "vei" "fxdwljs" "myjuba" "elbsib" "pvy" "xxjxgi" "dtgv")
    (list "linzaeu" "qbwdke" "fdg" "pykw")
    (list "qvtdd" "aco" "aav" "bpu" "mvkcuc" "kjfj" "japgfki" "jfdl" "gem" "hog" "bdzsiea")
    (list "wpbigkb" "lzhwba" "jssjkn" "qvb" "kmwu" "qddv")
    (list "iny" "osyvqnt" "tumunzb" "torq" "bdeneg" "wywank" "poza" "ipp" "iggorw")
    (list "tuko" "mhdbsf" "vmjdop" "jomaqpj" "rcdsud" "hmgspr" "lsas" "nzmwc")
    (list "cirkjq" "nmjuu" "xtgejv" "gtexvj" "vjcmtqq" "unjmu")
    (list "xsdmezq" "xvqjvqp" "exhygy" "qahju" "hvd" "qadmdh" "lok")
    (list "wvvys" "kax" "rohrrar" "rwhnvi" "lhnmefp" "lsktouy" "bxilosp")
    (list "wayf" "diobnl" "zvu" "obnidl" "oibnld")
    (list "cewil" "ygsf" "ffzp" "ruxhu" "vah" "lnvwt" "aef" "lnnjc" "kgkb" "gxtlx" "feko")
    (list "uti" "epphrin" "pywths" "cpzzh" "csjei" "nczhamy" "gayxmb" "bdcytq" "xkx" "fgmt")
    (list "qvzyuwi" "dwo" "swkw" "bwjdrn" "dasgd" "ijgw" "vzabaop" "yefyhmc" "wgij")
    (list "dyg" "sugrf" "vid" "etz" "weyqg" "nyntx" "dwfgwm" "khon" "hnzzzn" "xfyra")
    (list "ofbh" "bdrsk" "rdrjj" "elaxvk" "jrjdr")
    (list "msxau" "rsocvx" "zxdda" "mxz" "lknl")
    (list "qktaywx" "dirpdbf" "unqnd" "wbrwkuu" "fvmqwl" "emxr" "big")
    (list "xwz" "kvsydc" "ayokjyy" "qiah" "omw" "neo" "htltxx" "fxhwqwj" "colqvbb" "sxmo" "ephfkex")
    (list "ncjxoaf" "fwjkc" "czmhv" "ylg" "axcjofn" "dvj" "bzqjku" "opvcr" "jiwzucg" "vmhzc")
    (list "gmmnrt" "zqar" "twdwrg" "qiwwki" "fcbr" "lixm" "hjdwwe" "moiva")
    (list "roinlxg" "cxeezve" "whannk" "cxeezve" "pyoj" "boweioy" "cpkgxsz")
    (list "qkct" "qso" "xlb" "xyy" "aellfet" "rzt" "cbboow" "devfb" "nih" "fhbfxzi")
    (list "qyc" "ltxia" "alixt" "atilx" "xtgrv")
    (list "svruz" "ufvo" "rvesnxv" "dik" "vzurs" "jjg" "idk")
    (list "xeudhrg" "hudn" "cilo" "ljplosb")
    (list "kpb" "oyzvywx" "vldko" "qhfkwod" "bkeutk" "zqcqug" "pbriu" "wqocos")
    (list "qkngzfy" "whobyri" "aze" "jvipdty" "ocirbep" "icqwc")
    (list "kzxxlab" "sjr" "zhymws" "xkbx")
    (list "nnxs" "gkwtld" "dwhkry" "snuibq" "dtdl" "aicug" "bhtlfzp" "qzk" "jctos")
    (list "regvro" "mxcq" "hqof" "yraucxi" "jhkol" "iuxineo" "pbtnk" "rfjwc" "szgjpr" "ndqqj" "vfgm")
    (list "yqrfox" "xoqrfy" "utbryu" "utubyr")
    (list "jdubjt" "wqrl" "wnk" "rlqw" "nwiq" "pnbn" "qinw" "uaff" "ftdo" "htfrav")
    (list "rum" "mur" "umr" "tij" "ovbahl" "losao" "imawwpb" "wadhww" "tbteyqc")
    (list "napxd" "kzeiqcp" "ppgqucm" "xkityt" "frq" "hugrp" "gjgtt" "gmuqppc" "zwqme")
    (list "xyuzs" "ysch" "howlzgu" "dkqppbs" "nvbiz" "mks" "mtxv" "vivouex" "uvawq")
    (list "ffe" "lfsn" "nlq" "mpulheq" "ikcfo" "wdtz" "cnwsbph" "zkib" "muu")
    (list "bqkxav" "wtecb" "lxwdhr" "kqbavx" "aqxvbk")
    (list "czwswqx" "ldkxapd" "pfwd" "bdkkj" "iqohla" "cwosw" "ihqpd" "pcc" "ckhabbn")
    (list "foiip" "hau" "rbqiyhh" "htm" "omeubgh" "symh" "evfcqg")
    (list "lqx" "xlq" "rsgf" "izu" "esetis")
    (list "npsrkdj" "fvulgkw" "eovw" "mzr" "uobcze" "azb" "tij" "ihoer" "ehori" "jit" "wknsqhm")
    (list "gnrksh" "xwggt" "oosi" "bpnmhx" "qqaa" "mpmryu" "jhzyz")
    (list "yad" "gbexqcr" "gbexqcr" "gbexqcr")
    (list "ldca" "xxhznn" "twyy" "ytwy" "zhxnnx" "xfmpi")
    (list "floioot" "kfyh" "dhibv" "ezyznar" "sfg" "sfg" "ezyznar")
    (list "cinioim" "iiocmin" "ypla" "aypl")
    (list "mhwcjbz" "dftuqsy" "wswop" "eizbf" "ptsd")
    (list "ehx" "mlh" "nfxgfkz" "uuw" "xftmn" "ptlkbo" "vsnyo" "ttwce")
    (list "oexvf" "orcg" "cncnkfk" "comvhl")
    (list "lqewsj" "lyulrcl" "efixd" "qvd" "fhznqnz" "yvrkwyi" "xmhgc" "vzbp")
    (list "dmr" "wrxqh" "thcm" "giomp" "rtvl" "ssc" "gwq" "rbklw" "hcmt" "fjvud")
    (list "teozhb" "dmzwfv" "qkq" "pvcqfqq")
    (list "hvlebc" "qqmg" "repxk" "zwrjdx" "ztruwb" "such" "tyligs" "ybg")
    (list "psa" "rqznokd" "lgc" "jstqres" "yiqt" "mbiody" "xazb" "xjuk" "dtb")
    (list "lea" "ncm" "rnh" "myzqzwm")
    (list "wjml" "eums" "ueflvbr" "cjpgnl" "qduunu" "zfxaai" "jwlm" "lprzzg" "vrn" "ttetyr" "sume")
    (list "uwkgeu" "uiahd" "plyewgi" "vveo" "nwhsitz" "mcitc" "uvk" "zsxehgs" "sewl")
    (list "lnbdrka" "sgtivn" "sozzq" "mgd" "vhxfnlr" "twrfpk")
    (list "gadphmk" "mbx" "lmlbrf" "tsnehnr" "lawdpm" "fnima" "gxgl")
    (list "umty" "vrn" "dpow" "fsnnpjv" "fsnvnjp" "nnsvpjf" "cioaio")
    (list "euu" "uue" "zeskmtk" "hob" "stekkzm")
    (list "ypqpri" "qwdju" "ypriqp" "iprqyp" "jnoxqa")
    (list "lkppi" "ingfxw" "wlulvp" "yhwrli" "nxwigf" "oyuhq" "ggfslx")
    (list "kdd" "ypvr" "pyvr" "waw" "vyrp" "khqq" "mamxca" "bapq" "gobfm")
    (list "iuq" "upvdpv" "zxef" "bfwns" "lmq" "lxswr" "kpsqo" "pwde" "iaaou" "nsw" "udy")
    (list "lgzo" "nil" "ovgrmt" "omgtrv" "jrqp" "pqrj" "lit")
    (list "uumyu" "iiakfj" "gvdtzz" "qbux" "yxn" "ejs" "dvlts")
    (list "hcm" "ghutxq" "zswi" "tmyhqef" "hgxtuq")
    (list "shkhkdk" "kad" "seubeax" "kdl" "mzu")
    (list "cpykgr" "skx" "rfhpor" "xsk" "moyhlai" "ogv" "ophfrr" "dxipuuh")
    (list "beyw" "jvrre" "opodn" "zdoajhx" "fhg" "ijs" "drczy" "drczy" "hjungq")
    (list "jrzieja" "gfg" "yzdn" "yxm" "wshibsn" "fgg")
    (list "xtylh" "vxscmvp" "rfymq" "uzhpyea" "spxcmvv" "dlni" "msj" "yxhlt")
    (list "eov" "awql" "miv" "miv" "eov")
    (list "mmvrfbg" "fjiyf" "hvqz" "zpuqmbf" "fszyuz" "ldfgni" "wemfjl" "fjjpl" "rbnpy" "rfb")
    (list "ppzpeh" "nam" "ntv" "xnchtyk" "hja" "hpepzp" "foj" "bibvx" "nmmdlff" "bsrkp")
    (list "qiy" "qiy" "umhlnh" "qiy")
    (list "tyds" "oepk" "wae" "tdsy" "sdty")
    (list "ukawr" "rkwau" "ghtjhm" "axy")
    (list "wtbjiv" "btjivw" "ewaf" "hwk" "ttq")
    (list "kdpun" "myve" "sqv" "rhvpy" "fnjwt" "puw" "ujhf" "thsp" "nkdadqr")
    (list "vyw" "wkkpdpy" "xlgz" "lmmcuve" "ncuq" "lmotk")
    (list "pmsfw" "vxd" "jpe" "qxlyasx" "ejp" "gwuv")
    (list "pmgyndm" "ezofbvx" "nicbwrw" "kwnlj" "yjvnas" "fdpkfo" "mqcsyhn" "pyjpf" "fbexvzo" "vkftm" "erl")
    (list "trmwvk" "rywuzoz" "hbidea" "kicohfz" "heidab" "deaibh")
    (list "sogf" "govd" "dknpk" "vxrvk" "rlm" "vwhjk")
    (list "xnxbfmw" "wguzrhd" "zbmkz" "piwppa" "mkbzz" "xvwrdgy" "flusfqb")
    (list "cgduq" "hbnwr" "xfx" "mrejb" "ckw" "zkbaihf" "cloow" "cwk" "wuvthv" "iwqctx")
    (list "vugx" "qbucd" "gxuv" "ocb" "cob")
    (list "ilmet" "fbelxxz" "qratdfn" "unoj" "hbc" "duv" "srmikz")
    (list "vnzuw" "zgpbqgf" "uzm" "thysyxd" "dinfh" "bgvr" "olungg" "ksd" "dsetwqz" "hpg")
    (list "omagsf" "zpr" "coa" "kknx" "bzithq" "pewp" "flvoz" "xiiq" "weojqr" "wpep")
    (list "aagj" "gcglqt" "gqcglt" "xbfx" "dhdx" "lbx")
    (list "pljq" "plxuscw" "ilh" "wfk" "lhi" "hli" "fouieyw")
    (list "hvnh" "zvm" "aqy" "dzitirm" "veq" "ctux")
    (list "lglhs" "aqibdii" "hjbn" "cfgc" "qrg" "pnbntcx" "owoks" "ebz")
    (list "jozngde" "lwne" "mbo" "omb" "fnyzvvj" "gndozje")
    (list "bbdgc" "igtdj" "uhahgp" "sqduko")
    (list "uuspedu" "fgnspm" "ewc" "slly" "jbs" "chl" "heanm" "abqijx" "kadvgxu")
    (list "akfsft" "skna" "kusjqr" "rkqujs")
    (list "erc" "vrljpu" "lruvjp" "lpvjur")
    (list "iors" "hcdr" "fsqtcj" "vop" "vmn" "dtqnz" "tov" "oscjlw" "cdrh" "ctfjsq" "lrnts")
    (list "fxp" "mczo" "sjlcxa" "mzoc" "jmsq" "hcxybow" "dmrr" "bcoxhyw")
    (list "aac" "ewraerq" "odmxpz" "aac" "aac")
    (list "zzio" "zebmxa" "szeej" "poordr" "gmi" "owwnnh" "xfx" "rzrab" "lfey" "jesze")
    (list "akc" "yyoj" "vqod" "drtne")
    (list "joxhvyf" "ymasnbr" "omouvq" "isxdrr")
    (list "qyi" "ayrkzu" "jsk" "vqvvno" "jkkuxi" "zufnnwu" "mrsszdf")
    (list "ocqi" "htfb" "tzjna" "cdt" "wkzhynm" "eergf")
    (list "yokzugl" "usyuqu" "qvotq" "uweqyow" "lygkzuo" "kpmqmb" "uglyzok")
    (list "glvshl" "imqv" "jrv" "xlpnsy" "gcg" "psj" "irtiamg" "wkl")
    (list "bjcpc" "nvyloa" "dkkan" "efj" "okubpc" "cxlowm" "eone" "kmpny")
    (list "cyxqys" "nmuaftv" "gqxj" "gtvsc")
    (list "beouh" "dioxiah" "kizdy" "hyi" "cozrray" "rave" "fqxmxmj" "gdm")
    (list "frjz" "amrsat" "lxvhzj" "azhevtu" "vxlzhj")
    (list "zwmnrk" "sbk" "txzrcsj" "sbk" "oosgfej" "cvh" "zuthibi" "onvwd" "sbk" "nhwpzq")
    (list "gzamt" "vraw" "kuk" "ugayl" "lyaug" "bww" "rwav" "ijah")
    (list "bdjirxg" "vifjr" "rhbxpa" "oao" "yrhjxoi" "pbn")
    (list "navb" "umesiys" "yhix" "phuhu" "aekkciu" "nlnsiq" "wjf" "idqdwp")
    (list "cmhw" "rsu" "urs" "ziprlfe")
    (list "kyhxitv" "cgty" "bnwjyq" "cygt" "sgjn" "pdab" "imarvhg" "yjbnqw")
    (list "axaa" "ejancv" "yau" "njpc" "jvwy" "bpft" "kwjvg" "qzrbvtm" "diu" "njpc" "bpft")
    (list "ambj" "upe" "rmqr" "yudbiqf" "krudp" "pqyf")
    (list "tnb" "mobnpv" "vep" "ohxoc" "cyip" "wxyccfo" "jrbi" "rwsws" "kls" "zlv" "oohxc")
    (list "fjh" "dmb" "hlbq" "bqc" "jhf" "kax" "suz" "fjjg" "rkpc")
    (list "wjnn" "byfirm" "goeyh" "xtjmdka")
    (list "tgyfxx" "hefpxln" "mveobqr" "yeo" "ftfn" "srt" "vim" "vlcu" "hevoi" "xtaaff")
    (list "imyql" "xotcl" "poql" "rlueapq" "bkwykm" "hlalk" "bkwykm")
    (list "gkec" "zff" "hbmtq" "rjxjbcf" "arerlu" "pvz" "cdaqi" "nijmhv" "uodwjh")
    (list "mpctof" "mopftc" "ksfbat" "sbkatf")
    (list "nvdd" "jub" "bvi" "kyggdbx" "nwtiok" "gjt" "mgsm" "dbhsn" "rzibgjm" "dvdn" "eqi")
    (list "ysd" "iirp" "dfgzza" "wiyeoou" "ysd" "ispkv" "bcqg" "wwzqgq" "xphse")
    (list "ntq" "ivposb" "gsd" "ezl" "tlkztp" "lez" "qyurp" "vxsmg" "dgs")
    (list "wijs" "rydbj" "onm" "usiyqzb" "hwrol" "giusanb" "kewukl" "yziuqbs" "doojam" "nom")
    (list "lfacyy" "xwwast" "truqtt" "tzneimn" "uxsydc" "ktu" "eqyaj" "ndszak")
    (list "ffleooc" "kikif" "fohgop" "aucy" "moubqxu")
    (list "iaxc" "pnwexdl" "ncy" "vmwm" "xrqoi" "wpgftq" "rofx" "utyzjuf" "stdxq" "twpgfq")
    (list "ppmlp" "etsvi" "cjdx" "poly" "ynx" "vfxpslg" "mqjo" "qnpsage" "flpsxvg" "jwsxiqt")
    (list "lbyhnb" "kflrpeq" "ssoti" "webxr" "embbjd" "kbnx" "ubzqco")
    (list "khhc" "vwuqzb" "ebocbko" "rwmonkz" "edfqn" "hzh" "qhncoq" "gbwdi" "wjeg" "ocwow")
    (list "ghzhd" "kcxblp" "lzwkkr" "gzhdh" "umk" "pblcxk")
    (list "wyajtw" "jiff" "ouylv" "sni" "lwhlrg" "avqjiis" "igzx" "wbl" "lhrwgl")
    (list "glhh" "kaxha" "tqii" "hwzx" "rgic" "kaxha" "rgyidmt" "qdgxfl" "ynjc" "oibfij")
    (list "bapj" "bix" "rjniw" "ynbql" "idlvnmt" "wynpzbl" "zlpuoix" "kvn" "kakwys")
    (list "aldpxxu" "iojxp" "rif" "xbyqtr" "jffdvy" "qnrq" "tqwsdiu")
    (list "ulssco" "ktbymjw" "bfj" "zhkg" "zgc" "ctyri")
    (list "ilrmq" "wfahcgk" "mrlqi" "bguad" "inj")
    (list "cjzc" "rekuy" "ifr" "wfkg" "sple")
    (list "cvjkp" "qbmumnp" "mprg" "ltmwxxh" "zpemtyb" "ozzssfd" "ksu" "mgrp")
    (list "nvc" "sxp" "mpkxz" "bhlctq" "hguaa" "yrdkm" "iwsgfg" "qjssh" "gobbies" "hucdh")
    (list "jdxrjw" "qmo" "qmo" "vobhnu")
    (list "dnjib" "wtjp" "rfdjqdj" "skpvrb" "vkwevb" "kxxovp")
    (list "fzi" "kicta" "zkuvr" "rfaawv" "ehklq" "cfdjsyb" "tukahwr" "zkuvr" "kicta" "ouq")
    (list "aba" "ytdguk" "gqmpn" "hvxabff" "hvxabff" "dckj")
    (list "fna" "wxyqhxd" "hvy" "khsu" "yypoyy" "lvvue" "medheua" "gim" "slf" "drdbeh" "ikihf")
    (list "jquz" "wwo" "wwo" "ghlz" "jrbvb" "jrbvb")
    (list "jwzvkl" "yjw" "ouwla" "yjw" "ouwla")
    (list "zsvlgyf" "rzqbtj" "qygynem" "ukdgjm" "lbsyh" "tmdzp" "fbcaim" "eymzr")
    (list "pvw" "sbs" "dvsa" "plmepl" "pwv" "ayxk" "vpw" "dwt")
    (list "inayadn" "pnti" "yzhxk" "azga" "gxq" "aznbciu" "gjnmyqm")
    (list "isgf" "ndqmk" "beyqq" "ebyqq" "srtzxo" "aiiw" "oqfuwp" "uoqwfp" "buejctv" "pxbk")
    (list "pzl" "irv" "tzvzdb" "wcy" "eszm" "ybwiw" "ycw" "riizifd" "iybww")
    (list "btpu" "cua" "azzqffy" "owcr")
    (list "ofwq" "sqlpzat" "lozdxlc" "aevjmpc" "lcolzxd" "wbbysn" "qwfo" "vcrx" "gdzgi")
    (list "dbpfmxu" "ydsxwl" "ijn" "svxtop" "csep" "ldqeog" "ffye" "zcrl" "soh" "aclw")
    (list "wyiyyhv" "vyhiywy" "obgi" "hiyywvy")
    (list "ddvaoc" "lhv" "spurn" "rgxyy" "onjw" "illvn" "yryxg" "xyyrg")
    (list "vid" "wdttqq" "kajr" "myip")
    (list "wolqlue" "phlunpt" "dcmmkfm" "sgxk" "dmmckmf" "sfng" "jlbsntq" "dxp")
    (list "zmneyho" "fswj" "xdgsjc" "oefwjdi" "htgxvbd" "tgqrq" "xodoa")
    (list "ynw" "bygqdnh" "hhmnkuw" "cojqrke" "qszzdjo" "orskwq" "mdfae" "asabn")
    (list "vvpm" "vkj" "pcxghao" "caoxphg" "axhblxb" "vvmp")
    (list "txox" "nzy" "eqn" "zgir" "dytsi" "girz" "ffa" "ugjjbzj" "brob" "fll")
    (list "kbz" "pukqbd" "fiwmuh" "umwihf" "bkz" "dvz")
    (list "vgs" "vejs" "vejs" "vejs" "mbkyjjy")
    (list "viqmnmu" "bitkyw" "nddnk" "dknnd" "cldnpp" "hipub" "plcdpn" "fdzzpb" "mmyomn")
    (list "ndylnfx" "gozlrx" "ngptk" "rnpteb" "wtacx" "xmtcjy" "xldha")
    (list "fey" "doyxis" "ampmtr" "ycqh" "syw" "cqhlj" "hnngx")
    (list "dijf" "nac" "tvkq" "ayo" "akbj" "lzmngdm" "wfxpn" "bpyvrf" "cvdqpa")
    (list "zsofz" "lhho" "hgat" "wqskga" "mnt")
    (list "mylwm" "zxsd" "omzpa" "waz" "hcrr" "lxmpq" "jsw" "sqtwak" "pzoma")
    (list "rwhgsgt" "ysdq" "ztihici" "mpwcawv" "alkqg" "wsxiwx")
    (list "snldn" "bcb" "anjdv" "cbb" "awsscc" "cqxult" "hjmjew" "mcycb" "fdpdg" "sesrh")
    (list "kukrqm" "fawafz" "qdim" "wyobtqx" "bnvjnqg" "dcvqxta" "yptr" "nnpu" "ughldqp" "duo" "zafwaf")
    (list "knb" "yjqb" "bscpnt" "nzg" "sqeu" "zkahna" "ttuf" "nsbtpc" "ixwit" "vucwj" "idix")
    (list "bfqyx" "xlnpc" "ijrxu" "zkqi" "kjxtahr" "fgag" "orusms" "adi" "bfqyx" "bfqyx")
    (list "dqddc" "ncbv" "bvfk" "hefikb" "dqddc" "hqjl" "otpx" "zfiu")
    (list "ntkv" "qunrzx" "eztzure" "ctt" "rjo" "bkdt" "znvd" "jwdf" "gqhf" "mmhrzgt")
    (list "zeavm" "hkbf" "rawqwuf" "pis" "dojlkt" "vnjhmi" "uvk" "cufmn" "qginezd" "xyut")
    (list "hnidzk" "chlctc" "yst" "pepd" "dxntbxg" "vqk" "daxfpmu" "wshyddl")
    (list "jgd" "vesqgo" "bdyqy" "igl" "ahstdm" "wjtd" "lrtkjsv" "tjsj" "sccxbih" "esn" "gkkzj")
    (list "iisiswh" "jll" "rhlaf" "jqwwgfa" "wmhyo" "izva" "vrg" "zjkak" "nlxxfer" "rvhx")
    (list "mkrkd" "jlqtpy" "ukstro" "ktuors" "wsj" "ynqpbp" "kpiyxzv" "nxeiwg" "xpzvkiy")
    (list "jbr" "gnct" "fwklekg" "cmfqnm" "ctn" "gqobrs" "kwht")
    (list "pztmjs" "yiffc" "kfhsblx" "yiffc" "yiffc")
    (list "biezil" "iiezbl" "bzeiil" "smocoju")
    (list "viiigm" "gmmmk" "yeiv" "dxzogro" "qsmzsur" "hukzwjn" "lcle" "syo" "mdj" "uruf" "rxfseu")
    (list "extchsd" "adeff" "ouikoj" "fyaclr" "rwwvqsd" "dooe" "tcxheds" "zrdqqhm" "fdoxv" "kbxi" "tlcj")
    (list "aycnydq" "qlxhka" "zoi" "shplo" "qll")
    (list "bfry" "lbwckm" "ltq" "rbfy" "gpn" "vojp" "ruj" "dpxcve" "geq")
    (list "svtvfwh" "lca" "lac" "qia" "vhwsftv" "nookdfz" "xgjiaf" "yvcdlt")
    (list "aspgqym" "fryuzhx" "bbydf" "tbn" "bwutsc" "fqgi" "zij" "lmxhog" "qnmse")
    (list "rbb" "gsys" "volnas" "onvlas" "lonasv" "vwjdso" "lnteapy")
    (list "got" "iauk" "kficn" "jvfuy" "yvoe" "jcxwui" "hyamqx" "mke" "mwh" "jcxwui" "hyamqx")
    (list "avutfi" "ggmha" "dkopc" "kothnnb" "syoi" "xsd" "wjedywy")
    (list "oziejyz" "yzeijoz" "hnthyn" "knj" "juuq" "qujtp" "kgq" "bymlnlf" "yicf")
    (list "zsejuy" "dybeap" "hvowmvn" "okxb" "yoi" "epadby" "cnzjk" "xfwprzc")
    (list "lacg" "iiix" "fblhxvf" "nrkkol" "lnafzw" "qspzsn" "gvdy" "ipj" "zub" "uouseo")
    (list "evukwkh" "ycjxxc" "lptwmf" "pmd" "izxdsos" "zrkavf" "pgjoy" "zwokg" "mpjiej")
    (list "vqw" "ijwoy" "eaw" "wvq" "svmcq" "ccxi" "nyub" "ynlq" "eqornax" "uprt" "pygfe")
    (list "plue" "okbbm" "btvm" "gba" "kutn" "jacjx" "ysqt" "lvx" "pcxxu" "qcf")
    (list "pyw" "ffjfudq" "bvk" "hsdwdva" "fjnivhf" "odbmw" "krpgrj")
    (list "hziesm" "bxa" "dceiwt" "tmvivjk" "snl" "fkh" "dahsxyx" "kqlhak" "lurtk")
    (list "xss" "sswyxrg" "yqff" "dbkx" "kbxd" "mpzbmnl" "bzplnmm")
    (list "uvz" "pjm" "ilrol" "pmj" "uzct" "ztcu" "brhkv")
    (list "heiz" "jcn" "syjt" "zfvlvaq" "aflvqvz" "amcjh" "rxnitw")
    (list "cxl" "nxvrn" "vjnz" "aewtr" "cxtko" "nnvcp" "ltptd" "adpxt" "zvjn" "fntklj")
    (list "aymmm" "tuirj" "hzngq" "zhbh" "paqs" "kvpfo" "aqsp" "kmo" "acprw" "sabrso" "kdqmp")
    (list "ndqjspv" "mmhp" "pndjsvq" "rti" "usm")
    (list "ije" "oad" "mvelyg" "jadz" "ekm" "dao" "zdcmv")
    (list "qwww" "tmwmdbb" "oxxfoza" "rgmf" "eonku" "brh" "gcgiuoi" "ojscn")
    (list "fjedeek" "ohlax" "fiydku" "rbnxpg" "wfivg" "cdgs")
    (list "axwbni" "hojye" "mwfe" "oyqknxp" "whdgfy" "ihku" "mbhr" "gagnz" "hehagxj")
    (list "hibautd" "blnayq" "lnayqb" "gepml" "mgpel" "qunw")
    (list "ircx" "oeb" "kujtip" "zbu" "ebo" "cmmn")
    (list "upyqvot" "wbponp" "hnn" "vav" "avv" "tvrky" "omm")
    (list "yzqsnf" "agbfsw" "dbxoya" "sfnqzy" "hqrxek" "qsnyzf" "oagyerm" "xxhukm")
    (list "xzvk" "mvcwz" "oujr" "hell" "hoe" "xexa" "dqlpqt" "xdqz" "ucola" "hsvv" "tcmybhl")
    (list "skldxr" "mzyol" "ybzyzd" "jnnxb" "rxncdy" "nkpwy" "fwlnsw" "omylz" "oiwieu" "fshv" "ngvha")
    (list "jkwqf" "yxrox" "hejfoq" "orxyx")
    (list "rijken" "xiwf" "mawqcfu" "erinjk" "jsi" "yyg" "mmu" "mdkfqb")
    (list "ornjes" "krp" "eornjs" "enjros" "pyqp" "nnwwjl")
    (list "wzd" "uqqo" "kyeli" "tikdle" "aykdjog" "uiz" "rbpnw" "mjxezf" "ihiz" "rlgyg")
    (list "cjm" "ajqgvkz" "kfgyy" "dmczlc" "mjc" "kxcm" "zctyqgh" "ymsk" "jwhqfd" "czpqgan")
    (list "vxkzvco" "owo" "qogj" "uyictoj" "kfr" "pyoo" "ejrru" "npluynx" "bvv" "jhhzu" "kuciwc")
    (list "eqk" "pcsly" "kelu" "arzgoe" "trfo" "fotr" "cuaax")
    (list "lagonw" "qvcssqz" "sdoklh" "uvovi" "sfrkmd" "hnvafj" "ltg" "wfjj")
    (list "viwbkm" "hpwe" "kzzwrbr" "axjtlq" "mznin" "wwpjg" "unlwur")
    (list "nuzorgo" "qfoz" "ydisca" "qxdfutv" "hzg")
    (list "nqgge" "tobtt" "hjocx" "ntyqyi" "rxzkynw" "wrnxzyk" "ciscy" "trjt" "ottbt")
    (list "yuii" "srawx" "gljxe" "eteogz" "kcu" "jlgxe" "tjik" "ktsnp" "agudqok" "jwol" "vfnyv")
    (list "vgicg" "dhnrmxz" "sjhozy" "hlalx" "rutwq")
    (list "nyoyoje" "kco" "hoyam" "hoyam" "tta" "iflud" "amh" "gdxcsj" "vqr" "fvsqcgv")
    (list "xdmbtph" "ueen" "cskerl" "rxjvpdc")
    (list "nricn" "addljzg" "obq" "rikez" "igq" "bxygkmv" "qmgojou" "uheubk" "qor")
    (list "snzd" "ztusvr" "vrstzu" "mceddga" "hgu")
    (list "vvrbfjg" "mcdhmsf" "ldtwl" "otuna" "gmjurrx" "jgrurxm" "rxmurjg" "yrioq")
    (list "iotkvo" "sftfvn" "vvoit" "lllju" "xvlg" "rdsb" "ywmdf" "mzxigu" "kzq")
    (list "sgqw" "gqsw" "lqfu" "wgqs" "xpiwou" "jurgucd" "azq" "wgaqpm")
    (list "ijntzi" "chlnfj" "yjqatz" "hjflcn" "vys" "ofq" "oqf" "oadthe" "jrfw")
    (list "mmc" "motjo" "vcwmod" "rpaszfk" "zgkkua" "bpja" "vjb" "htrk")
    (list "bpfvvka" "kmger" "mnvvfl" "hakudy" "yfprdoo" "mvnlfv" "rgmek" "evnwg")
    (list "mykpu" "juavkn" "cecdvi" "aszbi" "lxm" "hmps" "oaqoif")
    (list "fshizd" "fsdzhi" "lvcq" "hhpb" "eavwno" "auqlwz" "rpv" "owcdojx" "amsmf" "qgnddd")
    (list "pohmcn" "hlcxk" "qsesxh" "rncr")
    (list "fgyrsis" "ldem" "avxmnh" "frpodq" "oefzn")
    (list "plfpu" "qdyojz" "xdrzrjy" "kpv" "abkh" "fge" "bbnotvp" "liikmcu" "czvwl" "oyh")
    (list "ovha" "muitw" "pzy" "edfjoo" "fhsxuh" "dliyruc" "dikcd" "cqem" "ywfy")
    (list "exyry" "jtzqn" "tscr" "qbtxno" "cikk" "poqgr" "tnjzq" "eofe" "sxea" "anlikep" "kick")
    (list "zcie" "purpw" "dmhhms" "bcdo" "prwup" "uprpw" "wfejgjd")
    (list "kwtjc" "cmixp" "dodfwj" "hcgmmat" "pkeyspo" "ubnl" "ajxvj" "ffkh" "xvw")
    (list "nvlgq" "oduus" "psufiqg" "lrwpn" "dleftn" "xtllqvf" "usgz")
    (list "liarf" "sczsf" "sczsf" "wky" "qtzq" "qvve" "qvve")
    (list "cit" "vtjsh" "jrhkyvi" "txj" "urmq" "hppx")
    (list "rhblmxn" "rhblmxn" "lkgow" "dylurwc" "beyk" "gfcewxj" "ehpl" "disoe" "tjbjy" "lkgow")
    (list "nbkrm" "jvk" "ffux" "ars" "agns" "bebic" "jzjfm" "kmnbr" "gptvtsa" "ufxf")
    (list "hrlvup" "jaz" "tafyr" "qcgq" "wkd" "fiz" "bgsrx" "jmtcvo" "qkbvj")
    (list "eontk" "djf" "tiafrng" "mtwat" "puainel" "nyjoh" "meynxbf" "eqdw")
    (list "aspvmbx" "tgzuszm" "fpj" "xkl" "nzpr" "fjp" "vnomk" "byx" "sbtov" "tnu" "utn")
    (list "ldyww" "gwmiddv" "hwyh" "gcgsdit" "gtgdisc" "suufl" "xsw" "dlwyw")
    (list "sye" "dgbd" "wyf" "ixqzthx" "dgdb" "esy")
    (list "nsdgera" "fqz" "xwbdgui" "ngdgbcd" "bcn" "qrdxml" "cwcmxws" "tncm" "mqsodj" "cqgk")
    (list "estayas" "cocmbpv" "cdcf" "vygtswo" "aplwa" "estayas")
    (list "ndc" "ndc" "wntr" "sfls" "sfls")
    (list "gse" "svv" "esmi" "lcdii" "lnr" "kemrk" "gnk" "ildic" "blnqy" "wvn")
    (list "mwlpm" "awkr" "sxsudub" "yauwww" "hnktbog" "fpnqc" "nmxoq" "yoparu" "tqjpkug" "nbipft")
    (list "czwnkk" "hrodtmx" "yyzpil" "ooqjb" "cvxzfh")
    (list "kwa" "wak" "gipak" "gsgrw")
    (list "jyy" "fja" "jjk" "kuvoqdy" "urqx")
    (list "doyu" "chgn" "gvtxi" "qjdigvy" "kxr" "dizwrjc" "sll" "zenl" "yyblj")
    (list "epxeqih" "kfi" "hlog" "pakk" "kkiidrh" "hiufw" "wuhif" "baqzxzi" "bgcd" "phi" "jzjdxjp")
    (list "hllhyad" "sodc" "nyrtfe" "kygof" "hyyqi" "txddqg" "wcwxvnt" "ewqmj" "wwv")
    (list "vxymuoe" "caat" "diqwbo" "vfruxdf" "sqniefn" "hetcbl" "nvtttu" "ouesb")
    (list "yvoez" "pvthzc" "tdowuci" "wjijicn" "fhpmq" "kfobag" "yctdwj")
    (list "xaugkb" "rprkg" "tidpx" "pjk" "tpwwm" "pbcfhr" "wmwpt" "sfynrl" "iouaw" "zbnyu")
    (list "auakc" "culuxg" "bffg" "rodyhea" "ixlmtfb" "jdurl" "szoa")
    (list "xgona" "fjzho" "buh" "khbvti" "ddh" "mgj" "ptgaqps")
    (list "dqldupd" "udpldqd" "poku" "gfgpcg" "zsvk" "grvk" "kntx" "jih" "uwvxdvq" "sivk")
    (list "mwdnq" "wmqdn" "uzto" "mdqnw")
    (list "alvfm" "qxqo" "thwru" "xqqo" "jilnsgs" "rnonk" "fwntuby" "ogbha")
    (list "gvxlxyf" "cdpv" "khvpka" "kgt" "gshlaa" "tenb")
    (list "mtgvvxh" "mrjrsd" "truk" "rrerzx" "tujweaz")
    (list "ozepw" "gsqkr" "rtmmc" "cmrtm")
    (list "spnthg" "xhlzuu" "xwcrxz" "aqqejhh" "bpzh")
    (list "ectdftk" "rgp" "mkp" "vxp" "pevriz" "wkgfkaw" "vfygj" "peg" "gep" "wjn")
    (list "bksbu" "ywsszf" "tsbrps" "vxicr" "hfustju" "ynnlbo")
    (list "sio" "urbvf" "ujezjk" "vkyc" "ukjezj" "bvrfu" "qwwgqmw" "uqfekvx" "bzipxus" "qfumwh")
    (list "druru" "kycweog" "ycmef" "rjyy" "fkgp")
    (list "rmf" "ifbip" "rsztco" "coju" "wlr" "bfbmsug" "lwr" "bsufbgm" "nwmp")
    (list "jjuxtyd" "yif" "rkldsvu" "binq" "spepa" "mfg" "aszm")
    (list "ghilaau" "ncm" "sgbavz" "omzeotz" "azukf" "bgjw" "zqzo" "gjbw" "pld")
    (list "gtog" "iqheik" "budeu" "guvljmi")
    (list "qqlj" "jqql" "ttk" "xcxu")
    (list "cfq" "cfq" "kpagib" "dxfxufw" "hhksbjh" "gpcp")
    (list "xkeax" "acnia" "jjubfc" "mhot" "uxlhh" "gnkj" "pavta" "rciondm" "rkquh" "xudqian")
    (list "wqhqzg" "psqh" "rnnc" "uujlgq")
    (list "hpjpaoa" "maa" "rdndl" "xewqj" "nmagwx" "xewqj" "hxuyvou" "xziv" "rdndl" "fbxmbz" "hmfwghy")
    (list "dtwnrca" "hbfcptw" "qrmvat" "sdatx" "les" "zwizogq")
    (list "bodiwzg" "sgoas" "fsf" "wgkrn" "zgbdowi" "wfkz")
    (list "ngcsg" "grtao" "wcfxpyl" "gngcs" "fxwycpl" "fkpt")
    (list "txvngo" "vxngot" "tkoap" "zqjc" "qzcj" "oeruix" "myh" "ihgdfik" "qtt")
    (list "rxeh" "fcbnoo" "rxeh" "lve" "wvoc" "pmnxej" "dlcbrh" "rztt" "noibg")
    (list "zyvq" "lwxqu" "oyjv" "bvidmf" "wxuql")
    (list "wzc" "zcw" "czw" "dnhkvrg" "nzslrf")
    (list "cfgl" "uwhxu" "qnsfmt" "tgyabes" "mqnq" "nkitq" "hmcvxlt" "qqmn" "yzmb" "uomqp")
    (list "lwziur" "hgmdmv" "zuvipkp" "vir" "apr" "gfaq" "zeo" "dunat" "mqgafzg")
    (list "prq" "pqkr" "xlrw" "njf" "ncqni" "kgpoma" "cmtklv")
    (list "jwfuc" "poz" "opz" "fuple")
    (list "fgleub" "lcgnifu" "lkwo" "kftbc" "onvwvdx" "lukpod" "xgmh" "rnj")
    (list "rwqvv" "ezjmoni" "llq" "ekd" "cdvv" "kzcci" "gzsj" "vuipv" "fnw")
    (list "rtnua" "gbnzg" "kqtogns" "iozzwc" "kjpzz" "kiiurey" "yzlvzx" "cpy" "xrue")
    (list "fexcjmw" "ebwssx" "ewbcgwd" "uwolou" "nfdhic" "vupiykn" "jss" "djoo" "xftbkgo")
    (list "idf" "ipvmez" "qyevwd" "wfsjxja" "dif" "dig")
    (list "szpbtsa" "bssaztp" "sptzasb" "qppgz" "odur" "cpmn" "wpmg")
    (list "pxn" "zjmq" "rbnr" "azwstzm" "mln" "upaqyty" "nxp" "oge" "nlm")
    (list "bfaryqv" "hag" "phtvh" "ypi")
    (list "epeeog" "lip" "zqio" "wuehlnb" "bau" "sbd" "dsb")
    (list "xbrrp" "sej" "agrqnpa" "aarpnqg" "bnwyi" "jbn")
    (list "uqmsvd" "asmuyy" "czxviw" "pznnmvc")
    (list "sddwmek" "wnaea" "iwphupk" "sabo")
    (list "cingdks" "ksh" "mtyip" "zltgafm" "dflkcd" "wbdnqup" "uokm" "gmxpyd" "libz" "svv" "akce")
    (list "qge" "ewv" "dkabkmb" "xcpi" "nrkmsu" "mkmb" "djvamg" "mhhrwjh")
    (list "krjt" "etfhm" "bxzatw" "zdkvz" "ehov" "seyxbw" "mkiirs" "plzoplu" "sogmwb" "wodfcle")
    (list "qwea" "adibdp" "emo" "homrd" "pjcrhlc" "eqaw" "kqsrp" "rphjlcc")
    (list "gajzo" "nwjg" "qxjra" "jztcnir" "ijvjwez" "avxb" "afz" "zyywqz" "kcszgh" "elmlkfh")
    (list "lbz" "ozia" "bctf" "bumoji" "anhil" "rta" "xvit")
    (list "ejybire" "ypjl" "qevak" "fzalx" "mlh" "qxlei" "zib")
    (list "xmzas" "kwojjz" "ntrnrw" "nbmxlv" "mdgxs" "xjhxg" "suo" "zdcrxl" "qkujisz" "pxmu")
    (list "eezyd" "unrtm" "wyu" "vhufvto" "rpb" "isfcy" "ygh" "hgy")
    (list "nszvbzv" "ebtt" "memrsva" "ebtt" "qwcaq" "bhbas" "pvzfbov" "ppjbdy" "nszvbzv" "jabvrp")
    (list "rlo" "zbmi" "lugvu" "yeby")
    (list "tfcd" "tvl" "faaq" "mnural" "nyarh" "xnxk" "ctdf" "bodz")
    (list "vwdrhc" "gub" "bgu" "fpcovx" "rcvwhd" "jukwsue")
    (list "aekrhi" "lpknnrh" "bett" "tkib" "ioqrap" "igwnst" "aekrhi" "lhha")
    (list "acg" "mknhazp" "pcgjuk" "tajplv")
    (list "masq" "fyjkn" "agq" "qhxbbl" "qga" "npzj" "fme" "xtihic" "rntisg" "iqv" "aqg")
    (list "ipagh" "fjth" "mswztpi" "iexd" "cocojy" "vhqrla" "joe" "wrsrmw")
    (list "njztu" "tsh" "auqrxca" "zpp")
    (list "jctn" "webxi" "haq" "irrr" "qox" "irrr" "webxi")
    (list "reaw" "axmnvd" "voakf" "lnz" "ftbxfh" "zjyxzl" "pryfjpv" "sistgb" "pov" "mshs")
    (list "gsy" "ctsngl" "ptmnyx" "vpjx" "zpvtori" "pfu" "ioycdrq")
    (list "aobdtlj" "osdnrth" "sgqe" "geqs" "qegs")
    (list "oamrlxk" "ygbb" "rkamoxl" "nztl" "sarbmtj" "yqupjt" "plu" "sbtarmj" "vpa" "rxea")
    (list "yvhgp" "yznko" "epwpza" "gqrsod" "rilukp" "cglhomj" "wnaplu" "ugvdko" "qdr")
    (list "cggztg" "ajw" "gggzct" "ubmiefj" "kpa")
    (list "rel" "lvasbh" "kobm" "mdnzla" "pwnyj" "ehep" "gzx" "nhjdnsg" "rxa")
    (list "qaz" "gook" "rplqwh" "vsht")
    (list "dhe" "aneq" "ivrn" "awekad" "ckcbt" "zsqca" "ehd" "rvni" "oulwfuu")
    (list "oxgzzow" "wntz" "tkqaoi" "oxgzzow" "lwkdpgy" "lhd" "aekjasp" "tkqaoi" "dnhaw")
    (list "alxghco" "cpanoa" "onjh" "hyeyebe" "whxn" "zfu" "zozbll" "gojn")
    (list "zdqulsa" "dlqsazu" "zqudals" "sfedw")
    (list "rydtrsv" "rrtvysd" "fvyza" "drdgh" "lsfzt" "blnxr" "cnxe" "tslzf" "iijyds" "ylcxn")
    (list "cczea" "nxx" "kwol" "kopaza" "wuvr" "cyvoo" "whlicv")
    (list "zbmrwdq" "tlzbevx" "jwzpsc" "uvkwpd" "bmss" "rbzblj")
    (list "jogx" "jgi" "gji" "hypmtkg" "ijg" "oscjv")
    (list "flkoqja" "kwmrqv" "wzehel" "fvmcfap" "mkwqvr" "ivwxg" "jqfwdvo" "hweezl")
    (list "vgjg" "nzucho" "nuohcz" "ggvj" "tmxci")
    (list "fqaqx" "zeybhtg" "bxeic" "lftuqp" "wzuerz" "sww" "qfltxk")
    (list "keiy" "myrvp" "blkxcg" "lncqmsu" "diittlg" "fqrf" "digrel" "cpwrk" "ipan" "dkxb" "bymlzo")
    (list "owm" "irygdz" "pyhj" "mow" "wmo")
    (list "noul" "pbvvt" "zcv" "ueqyjl" "zhetlw" "lpjfhli")
    (list "felvwb" "wdykz" "kyibdz" "haq" "qkouj" "vuav" "oztyqh")
    (list "dyxo" "njcr" "hcuk" "ysrr" "pucw" "qbajztc")
    (list "ooyaz" "pmt" "hqwu" "gjx" "tmp" "tpm" "pwz")
    (list "lyhzajz" "dfot" "avyifo" "kdwka" "pwypcep" "kyyw" "tirlku" "zdpjmft")
    (list "aexle" "hfxo" "dacwvcy" "xsiotyg" "cifq" "ibupshj" "aktt" "rzvf" "pgafj")
    (list "pxubhw" "ibpm" "jxtxg" "iwnssf" "osbpj")
    (list "exmtfyx" "blbfg" "emrunru" "zkuhoi" "lfzn" "zrj" "unmcece" "phuppi")
    (list "icomb" "rmy" "mvsqqkh" "zwjubz" "lumq" "wekx")
    (list "cmdgs" "gsr" "pfhqx" "pfhqx" "cmdgs" "pga")
    (list "rpyf" "jejc" "adaiou" "dutv" "imbenyu" "dqw" "zhebjhu" "pryf" "vtxs" "yprf")
    (list "cxj" "roprjn" "rqoh" "qacagru" "snxd")
    (list "rczvi" "hfpl" "luc" "yowgj" "nvavlhw" "vjudkmv" "dwu" "teq")
    (list "klwc" "cktzh" "ksnvswl" "nsgeu" "xyohp" "mhs" "fxnjhm" "fwrcg" "rdeadkx" "cim")
    (list "ounvb" "vzqje" "ujctzzk" "iyy" "vxck" "ebtvbqr" "uswsmcr" "jveqz" "qejzv" "jmi" "pboq")
    (list "lwffygh" "mqsh" "vnnj" "ufz" "qhms" "gqfuxo" "lurzmu")
    (list "buf" "psdluck" "gapwoo" "wgll" "sbfavbc" "lljfvzx" "cdgo" "rpt" "sfvabcb")
    (list "svefr" "kubbri" "fervs" "nboi" "zkvq")
    (list "jwr" "vtc" "zkcpzb" "kczbzp" "cdned" "pzbzkc" "wigjuak" "fszgweu" "odflfek")
    (list "vwdqm" "khnnj" "plokjg" "vnce" "venc" "vecn" "yzxtgb")
    (list "tawl" "yrhoz" "tawl" "yrhoz")
    (list "vvehsl" "kdhzgme" "rix" "rcs" "btm" "pxnlsps" "vlhesv" "sxpnslp" "yqjtool")
    (list "eqpyw" "kpmkcyw" "wqhglxg" "ajfzo" "hbd" "qvmhy" "nhokah" "iisqvad" "kxuyd" "fxek")
    (list "jsz" "txhwhah" "hxt" "djnvl" "srylveu" "pxp" "dzmmn" "epek" "tzs")
    (list "joyzql" "jqczueb" "rtdyw" "fyc" "fjirfyn" "tjcalz" "joyzql" "fyc")
    (list "pjrmiz" "xwnmwns" "kcqjuut" "zfgxhdr" "octwn" "kqppg" "zhfgxrd" "wmwnnxs")
    (list "ema" "yqxqs" "aljjo" "ajloj" "wozb")
    (list "urgmhiz" "epqj" "vhhaxdm" "ptlsvig" "qzbmm" "cumbho" "lkg" "gyzmg" "eaopyzf" "ncfy" "mqe")
    (list "ijvwvo" "oszkees" "ugvyk" "hjdj" "ftip" "itfp")
    (list "ylfw" "qutzdj" "mgqp" "cyjss" "yzsdqqi" "iykvs" "fyor" "sthyqp" "mrjtzee" "hgo" "zwqbtgk")
    (list "bkfkns" "gco" "bykzc" "mje" "dwmkrwt" "ljegqor" "yxjxp" "oaleuu")
    (list "xeltq" "ggyqis" "aud" "frtyxhx" "iwz" "wiz" "fwoxz" "fozxw")
    (list "zdu" "nwduqsa" "nced" "iphaaxo")
    (list "bqjj" "oah" "ezd" "brhgxrc" "pmkz" "kdog" "exw")
    (list "ihatt" "hck" "iepn" "egemprp" "wrz" "wzcuo" "xjzeaa" "wku" "ivjvihh")
    (list "cwkuof" "bmj" "qmxd" "qbtms" "zgdei" "bsqmt" "ssndhw" "eeenku" "lcsqy" "bvvodr")
    (list "tek" "zsgytci" "vgoun" "kwwu")
    (list "jcxvp" "ijxc" "buqgix" "uil" "zfoku")
    (list "ggndshq" "bmjeo" "yqaxtik" "blspz" "yofh" "edaroy")
    (list "ipvtxh" "ouye" "elln" "dllvx" "iqza" "nhwf" "zyfw" "pvlky")
    (list "iydcx" "gvarm" "gvarm" "wegmiy")
    (list "sfjd" "liiflle" "mulboe" "qywzs" "tzbns" "trojl" "pad" "mnfcrhb" "sltb")
    (list "gthqj" "jvpsof" "jwlfyeg" "jwhlfj")
    (list "qckv" "umzrge" "gnzc" "mnr" "xde")
    (list "gvgxmhv" "txnait" "taxint" "ius" "iboqdj")
    (list "vsfex" "kbpvsby" "qembkb" "efxvs" "vhflzvm" "eaazg" "dyg" "bbmekq")
    (list "wxpfk" "xwfpk" "xwkpf" "cjsyi")
    (list "knzg" "eefq" "feqe" "seppop" "ttxz" "qnqfn" "atgsy" "cch" "mkjlbwt" "uyhct")
    (list "quzw" "jbiw" "miqehe" "qvf" "jyipqh" "kzcjxyh")
    (list "teuvzf" "tdtwoi" "pcuafa" "cwgjk" "ccur" "lgmqv" "jpjdkk" "efrnw" "uloqn" "dpkjkj" "lwloeph")
    (list "yaffjy" "xntstsv" "gygq" "sxttvsn" "tvnstxs")
    (list "cvbmdf" "pfrfkna" "wupv" "van" "iocb" "hsiyke" "obspj" "ytyfkl" "hbsqtij" "hkcw")
    (list "oeddmnu" "koso" "mdodeun" "ybe" "mhjbmwy" "ubejz" "soko" "yxvuv")
    (list "nylhy" "ylnyh" "olb" "vcdik")
    (list "gsp" "ilba" "llnu" "jjk" "urbvuma" "qzypf" "bkceotg" "ezxq" "hyvjngf")
    (list "tfnegyq" "rue" "waeif" "tfnegyq" "mvqm")
    (list "wvgnsk" "cpd" "oib" "wrdfaz" "kohwgkc" "kzzig" "hogkwck" "gkizz")
    (list "fecuuyp" "yfq" "bvanvxb" "cjeqwf" "unw" "dccr" "qzh" "zqu" "voakj")
    (list "utoazh" "bjuq" "kmhcre" "izmny" "mirorsy" "twnl" "jyoc")
    (list "fnnpd" "dmr" "ccgu" "eqgewc" "zuqivf")
    (list "kkxiba" "qdabuen" "oikaz" "dnuywmm")
    (list "aogud" "adugo" "uzcglpj" "lucv" "dgoua" "mdsqa" "mvrg")
    (list "lymhv" "sof" "hvyml" "mlvhy" "nit")
    (list "chu" "bwxp" "xpbw" "ghaix" "seklnc" "ola" "zofnrwt" "uch")
    (list "wtt" "abob" "vblijtd" "oabb" "qjws")
    (list "uozrpw" "kgf" "gxidxm" "uehdr" "fta" "pqakkrq" "atf" "fat" "woaolk")
    (list "gaee" "voshd" "ghlyy" "emvzlkg" "cmcgk" "tuwlsj" "jwtsul" "znrta" "mjieqph" "glker")
    (list "qiugxas" "gkg" "cbzmoz" "kahs" "obzzcm")
    (list "puz" "omcokz" "gjc" "heuqb")
    (list "dgndhb" "wid" "wdi" "scwnrjf" "juaisgo" "eivaw" "hgdndb")
    (list "mgcrd" "hnqg" "pkpeb" "vprxcp")
    (list "atlcnzp" "fyp" "cpkivxi" "bzj" "ypf" "cqpt" "bysu")
    (list "pnd" "jiitmzs" "csw" "mxnpck" "vxutdrs" "ivipzy" "cws" "xiegsy" "qut")
    (list "txlk" "avcvbuu" "hnq" "yyriq" "ajyswd" "urgiwc")
    (list "qgiqut" "gvblizs" "giqnfrk" "tty" "mvoj" "wpikl" "giqnfrk" "bkdpndu" "xztmxn" "hsmqxf")
    (list "llthg" "zjslki" "wilj" "rcyfois" "bavz" "hrqxn")
    (list "ytbw" "hlkl" "vip" "skycogy" "ejiirhx")
    (list "ndmtg" "bthlbw" "lsoq" "cvlvo" "sqol" "sqlo" "bppl" "sdkbls" "dtpyzrq" "vgm")
    (list "psm" "xpj" "xjp" "lqi" "spm" "gqirw" "aglpj")
    (list "htg" "fcchvyt" "xffev" "szdu" "lieadft")
    (list "nbjo" "qohgzu" "vofg" "vvild" "dbtyi" "pdolxn" "plnoao" "jxze" "xlpbxj" "brajzg")
    (list "urpp" "jjv" "lihmvp" "ivkwdqr" "sesyp" "ypbry" "qok" "sesyp" "ivkwdqr" "was")
    (list "yinepzv" "qvnzdtf" "apv" "ucxo" "bdioo" "juga" "hjfsyl" "hmowo" "avc")
    (list "dmiv" "tplae" "iiuiaxx" "tpale" "pyzkc")
    (list "giwhst" "mpexd" "byfyc" "swuzkc")
    (list "yydkwp" "xuu" "vjya" "kav" "ujmcxy" "qrtp" "zvlk")
    (list "lsvdyn" "tkw" "qxu" "omvlc" "wwmfvov" "mrgcoov" "dhpu" "tfair" "hupd" "zbx" "njzgwtw")
    (list "zuz" "rsxc" "xsrc" "gdwwf" "nycsv" "zzu" "kcu")
    (list "unlvzv" "jerqqgm" "nozma" "ykbflj" "qihqkx")
    (list "pctffo" "begf" "ivrvy" "ezru" "mvqt" "waocq")
    (list "tubtuk" "gxkc" "ikgw" "bjrird" "kxjebbh" "sbjyc" "yafkd" "khqajmt" "aclpmf" "gqfo" "yrpf")
    (list "rdt" "vrxa" "fyudo" "myeosb" "ursflwk")
    (list "wbjras" "edlbwdp" "ctobtw" "jbvtvcd" "xjgoo" "cmunxm" "mjtbpi" "klovx" "bypmsab" "unc")
    (list "xckml" "uztr" "htublq" "vilabvr" "jdiwus" "qejxur" "evfw" "qqm")
    (list "tzqq" "tzqq" "wkb" "wkb")
    (list "dgmg" "ljzc" "dgmg" "mbmco" "cgze" "qsap" "jccvot" "uors" "iiq")
    (list "rwvac" "woylk" "dmn" "teorprx" "nyuvz" "hcwwxlj" "lvej" "drbjo" "asjgq")
    (list "ljen" "tpfl" "vixcivr" "guaf" "lnje" "waim" "jlen")
    (list "djgaa" "janhi" "adudm" "yzv" "zkcb" "xqw" "fgvrz")
    (list "kpkjoon" "ggzx" "skp" "rqcsw" "xgzg" "zgxg" "jtf" "ghc")
    (list "rtnyxo" "qixfd" "nphekk" "mouzk" "gny" "fpzquw" "qgywx" "rpr" "gqydze")
    (list "gawdlv" "vrivoof" "rte" "iyp" "gaih" "sfzplm")
    (list "csojx" "wzojode" "uzy" "qulr" "lylmb" "guvtkwv")
    (list "ovxj" "aamms" "ftxo" "ebckdqw" "wqvsdci" "jwfqxks" "jafrcrn" "yyomrot")
    (list "qnu" "jqwr" "ywudxk" "qpsez" "rdc" "kiyfz" "iiecf" "dthxjjb" "bown")
    (list "typ" "zxcvjo" "rip" "acjhl" "paaab" "qhqipg" "xkguye" "sbxy" "pomkvn")
    (list "ofvaegv" "hgak" "oafevgv" "hkemar" "rqkha" "grklnsp" "msvkkku" "rekahm" "bxmjnw")
    (list "ahoihju" "sdyn" "phi" "uhz" "lupbx")
    (list "lavt" "jef" "klmq" "oqyfpf" "kis" "nazul" "ymezxek" "xpla" "fxyrfnt")
    (list "nwnagwy" "hvpjqfg" "sgm" "ungfstr" "gso" "owqqxjh")
    (list "hey" "hye" "ipyrt" "qxmthg" "jth" "wpbr" "hxgmtq" "cvfkfux" "qykdzhk" "movcfnl" "vxyoc")
    (list "zsras" "abnrj" "fgaczuk" "ssazr" "xzf" "cnxu" "gns" "wnqqy" "dwjh" "szars")
    (list "uhb" "zanlvh" "lvdotkb" "xekl" "kcofo")
    (list "lhx" "iccy" "ibkjw" "ciykxaj" "imsx" "ehamqlz" "iwzapxc" "rhaltv")
    (list "pofit" "owmpqej" "vwrobh" "jvox" "gdqehss" "yyxd" "styu" "tfkm" "fiotp")
    (list "ecz" "mdpoqsv" "mdpoqsv" "yxx" "rexok" "hcfll" "yvury" "hdhcfu" "juhkvpt" "rspnfj" "hxvgdir")
    (list "ohed" "mtigaoe" "eodh" "agmiteo")
    (list "vjvv" "hfco" "cppbxtw" "hawsjxz" "ovlsq" "qgs" "risgwhg" "auhj")
    (list "togivgg" "czrtvw" "ccz" "wzvtrc" "bse" "lsk")
    (list "ndc" "ndc" "lrfi" "iyleol" "nchx" "jxpv" "xdcsfmp" "nnx" "wtvq" "pih" "tgc")
    (list "hzpf" "sur" "zhfp" "klfmhx" "lbuidp" "xiqimnf")
    (list "qddpdk" "trfxpip" "pnsowj" "hidgvnf" "prur" "rsrautp" "aamykfm" "fysqjmq" "xwzjane" "mbmtxhf" "oqctt")
    (list "lfd" "eops" "govslp" "ultbye" "vrqai" "hcjkcf" "snpape")
    (list "cbok" "koumkad" "otpozb" "pqcs" "emilpe" "wpcyvxd" "bock")
    (list "spjb" "xkkak" "anuvk" "ejoklh" "nyerw" "bsjp" "zxuq" "vcwitnd" "xxtjmjg" "zfgq" "xkpf")
    (list "juo" "pmiyoh" "xxk" "myphio" "ogfyf" "dovlmwm" "moevao" "qqxidn")))

(print (day-four-part-one *input*))
(print (day-four-part-two *input*))
nil
