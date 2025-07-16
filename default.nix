{ mkDerivation, base, bytestring, directory, filepath, hspec
, hspec-discover, interpolate, lib, markdown-unlit, mockery
, process, silently, string-conversions, text, transformers
}:
mkDerivation {
  pname = "cradle";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring process string-conversions text
  ];
  testHaskellDepends = [
    base bytestring directory filepath hspec hspec-discover interpolate
    markdown-unlit mockery process silently string-conversions text
    transformers
  ];
  testToolDepends = [ hspec-discover markdown-unlit ];
  homepage = "https://github.com/garnix-io/cradle#readme";
  description = "A simpler process library";
  license = lib.licenses.bsd3;
}
