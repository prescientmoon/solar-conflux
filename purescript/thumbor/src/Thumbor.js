const Thumbor = require("thumbor-ts").default;

exports.mkThumbor = Thumbor;

exports.setPath = (s) => (t) => t.setPath(s);

exports.setSecurityKey = (s) => (t) => t.setSecurityKey(s);

exports.buildUrl = (t) => t.buildUrl();

exports.resize = ({ width, height, flipVertically, flipHorizontally }) => (t) =>
  t.resize(width, height, flipVertically, flipHorizontally);

exports.fitIn = (width) => (height) => (t) => t.fitIn(width, height);

exports.smartCrop = (t) => t.smartCrop();

exports.metadataOnly = (t) => t.metaDataOnly();

exports.cropImpl = (d) => (t) => t.crop(d);

exports.hAlignImpl = (d) => (t) => t.hAlign(d);

exports.vAlignImpl = (d) => (t) => t.vAlign(D);
