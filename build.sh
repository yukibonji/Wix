#!/bin/bash

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

if test "$OS" = "Windows_NT"
then
  MONO=""
else
  MONO=mono
fi

[ ! -e ${DIR}/.paket/paket.exe ] && $MONO ${DIR}/.paket/paket.bootstrapper.exe

$MONO ${DIR}/.paket/paket.exe restore

$MONO ${DIR}/packages/FAKE/tools/FAKE.exe "$@" build.fsx
