FROM haskell:7.8
MAINTAINER Lyndon Maydwell <maydwell@gmail.com>
RUN cabal update
ADD . /opt/bramble
RUN cd /opt/bramble && cabal install -j4
ENV PATH /root/.cabal/bin:/usr/local/mercury-14.01.1/bin:/Users/lyndon/ghc_versions/ghc-7.8.3/bin:/Users/lyndon/bin:/usr/local/heroku/bin:/Applications/Postgres.app/Contents/MacOS/bin:/Users/lyndon/Code/apache-maven-3.1.1/bin:/Users/lyndon/ruby_versions/ruby-2.2.1/bin:/usr/local/texlive/2014/bin/universal-darwin/:/usr/local/bin:/usr/X11R6/bin:/opt/local/bin:/usr/pkg/sbin:/usr/pkg/bin:/sbin:/usr/sbin:/bin:/usr/bin:/Users/lyndon/.cabal/bin:/Applications/MacVim.app/Contents/MacOS:/opt/local/bin:/usr/local/git/bin:/usr/local/sbin:/opt/local/sbin:/usr/local/go/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin:/usr/local/go/bin:~/bin:/usr/texbin:/usr/local/mysql/bin:/usr/local/mysql/bin/
WORKDIR /opt/bramble
CMD ["bramble"]
