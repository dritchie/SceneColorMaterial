toolsdir = 'C:/Users/sharon/Documents/Color/JuddSaliency/';

addpath(genpath(toolsdir))


indir = 'C:/Users/sharon/Documents/SunDatabase/Segments/users/antonio/static_sun_database/l/living_room/';
outdir = 'C:/Users/sharon/Documents/SunDatabase/Segments/users/antonio/static_sun_database/l/living_room/saliency/';

%Now run the saliency function
imagefiles = dir([indir, '*.png']);
nfiles = length(imagefiles);    % Number of files found
for ii=1:nfiles
   currentfilename = imagefiles(ii).name;
   saliencyMap = saliency([indir, currentfilename]);
   
   outname = strrep([outdir, currentfilename], '.png', '_Judd.png');
   imwrite(saliencyMap,outname,'png')
   
end