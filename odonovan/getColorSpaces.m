function [hsv lab chsv hsvRemap] = getColorSpaces(rgb,hueMapping)


%lab=colorspace('rgb->cielab',rgb')';

[lab(1,:) lab(2,:) lab(3,:)] = RGB2Lab(rgb(1,:),rgb(2,:),rgb(3,:));

% lab = lab ./ repmat([100 128 128],size(lab,2),1)';    %rewrite for speed
lab = lab ./ [100 100 100 100 100; 128 128 128 128 128; 128 128 128 128 128];

hsv = rgb2hsv(rgb')';

%HSV cartesian
%remap hue
hsvRemap=hsv;
hsvRemap(1,:)= ppval(hueMapping,hsvRemap(1,:));
chsv = [hsvRemap(2,:).* cosd(360*hsvRemap(1,:)); hsvRemap(2,:).* -sind(360*hsvRemap(1,:));hsvRemap(3,:)];

