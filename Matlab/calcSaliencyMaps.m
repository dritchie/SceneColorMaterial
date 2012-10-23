%toolsdir = 'C:/Users/sharon/Documents/Color/JuddSaliency/';
%addpath(genpath(toolsdir))


indir = 'C:/Git/SceneColorMaterial/paletteExtractionWorkspace/';
outdir = 'C:/Git/SceneColorMaterial/paletteExtractionWorkspace/saliency/';

%Now run the saliency function
imagefiles = dir([indir, '*.png']);
nfiles = length(imagefiles);    % Number of files found
for ii=1:nfiles
   currentfilename = imagefiles(ii).name;
   saliencyMap = saliency([indir, currentfilename]);
   
   outname = strrep([outdir, currentfilename], '.png', '_Judd.png');
   imwrite(saliencyMap,outname,'png')
   
end