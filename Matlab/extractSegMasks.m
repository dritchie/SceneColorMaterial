% % 
% LabelMeToolbox segmentation and mask saving script
% Put this script in the LabelMeToolbox directory (or add the
% LabelMeToolbox to the path)
% 
% This script will save segmentation maps to HOMESEGMENTS
% It will also save object masks and bounding box crops to HOMEMASKS
% HOMEIMAGES is the directory for the SunDatabase/LabelMe images
% HOMEANNOTATIONS is the directory for the SunDatabase/LabelMe annotations
% 
% There may be some issues with the directory structure and whether or not
% the prefix 'users/antonio/static_sun_database' is in the annotation
% folder name
% %

% My directory parameters
HOMEIMAGES = 'C:\\Users\\sharon\\Documents\\SunDatabase\\Images';
HOMESEGMENTS = 'C:\\Users\\sharon\\Documents\\SunDatabase\\Segments';
HOMEANNOTATIONS = 'C:\\Users\\sharon\\Documents\\SunDatabase\\Annotations';
HOMEMASKS = 'C:\\Users\\sharon\\Documents\\SunDatabase\\Masks';

% Just look at one folder for now
D = LMdatabase(HOMEANNOTATIONS);
folderquery = 'users/antonio/static_sun_database/l/living_room';
[Dquery j] = LMquery(D, 'folder', folderquery);
numImages = length(j);

for i=1:numImages
    % Check if there are objects annotated, if not, skip
    if (~isfield(D(j(i)).annotation, 'object'))
        continue;
    end
    
    % Save the segmentation maps
    % find the right image resizing parameters
    imgtmp = LMimread(D, j(i), HOMEIMAGES);
    [owidth oheight channels] = size(imgtmp);
    maxDim = 500;
    width = owidth;
    height = oheight;
    if (owidth > maxDim || oheight > maxDim)
        if (owidth > oheight)
            width = maxDim;
            height = round(oheight * maxDim/owidth);
        else 
            height = maxDim;
            width = round(owidth * maxDim/oheight);
        end
    end
   
   [img, seg, names, counts] = LM2segments(D(j(i)), [width height], HOMEIMAGES, HOMESEGMENTS);
   S = mat2gray(squeeze(seg));
   imgresize = squeeze(img);
   
    mkdir(fullfile(HOMESEGMENTS, D(j(i)).annotation.folder),'segments');
    resizename = fullfile(HOMESEGMENTS, D(j(i)).annotation.folder, strrep(D(j(i)).annotation.filename,'.jpg','.png'));
    segname = fullfile(HOMESEGMENTS, D(j(i)).annotation.folder,'segments',strrep(D(j(i)).annotation.filename,'.jpg','.png'));

    % Save the resized images and the segmentation map (for palette extraction)
    imwrite(S, segname, 'png');
    imwrite(imgresize, resizename, 'png');


    % Save the object masks
    % Make a folder for the scene 
    %imgtmp = LMimread(D, j(i), HOMEIMAGES);

    %Skip the large images for now, not sure how to make it work for
    %resized images right now
    if (size(imgtmp,1) >= 2000 || size(imgtmp,2) >= 2000)
        continue;
    end

    mkdir(fullfile(HOMEMASKS, D(j(i)).annotation.folder));
    [mask, class] = LMobjectmask(D(j(i)).annotation, HOMEIMAGES);

    % Make a folder for each class, then label with object id
    for c=1:length(class)
        fname = fullfile(HOMEMASKS, D(j(i)).annotation.folder, strrep(D(j(i)).annotation.filename,'.jpg',''), class{c});
        mkdir(fname);

        % Find the bounding box
        [x, y] = find(mask(:,:,c)>0);
        minX = min(x);
        minY = min(y);
        maxX = max(x);
        maxY = max(y);

        % Save the cropped image
        imgcropped = imgtmp(minX:maxX, minY:maxY, :);
        imwrite(imgcropped, [fname, '/', num2str(c),'.png'], 'png');

        % Save the image mask
        imgmask = mat2gray(mask(minX:maxX, minY:maxY, c));
        imwrite(imgmask, [fname, '/',num2str(c),'mask.png'], 'png');
    end
end

