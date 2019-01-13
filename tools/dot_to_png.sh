#!/usr/bin/env bash

# Transforming the ACDFG file
dot -Tpng -O `pwd`/$1/provenance/$2.acdfg.dot

# Transforming the CFG file
dot -Tpng -O `pwd`/$1/provenance/$2.cfg.dot

# Transforming the CDFG file
dot -Tpng -O `pwd`/$1/provenance/$2.cdfg.dot