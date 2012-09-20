INSTALLING NLTK (Python Natural Language Toolkit)
--------------------------------------------------

Follow directions here: http://nltk.org/install.html

You must install python 'for the current user only.' Python module installers look in HKEY_CURRENT_USER to find your python installation, so if you install python globally, module installers will complain that you don't have python and will fail to install.

Contrary to the above site, the numpy installation is *not* optional. If you have 64 bit Windows (who doesn't these days), then do not install the version that's linked from the sourceforge page--this is a 32 bit version and shit will break if you mix this with your 64 bit python / 64 bit other modules. Instead, go to this link to find an installer for 64 bit numpy: http://www.lfd.uci.edu/~gohlke/pythonlibs/#numpy


GETTING WORDNET DATA
---------------------

Follow the directions here: http://nltk.org/data.html.

Navigate to 'Corpora' tab, and download the 'wordnet' corpus.
