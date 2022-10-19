close all;
%S=[1 2 ;2.5 3.5;3 4; 1 8;2.5 8.5; 3 9];

figure;
image(Image_ROI_T,'CDataMapping','scaled');


save_3D_matrix_as_gif('sagital.gif', Image_DataS, 0.17);
save_3D_matrix_as_gif('transversal.gif', Image_DataT, 0.17);

Data=reshape(Image_DataT,64*54,20);
ind=classification_spectrale( Data,5,2.8);

result=reshape(ind,64,54);

figure;
image(result,'CDataMapping','scaled');
colorbar


figure;
image(Image_ROI_S,'CDataMapping','scaled');

Data=reshape(Image_DataS,64*54,20); 
ind=classification_spectrale( Data,7,0.49);

result=reshape(ind,64,54);

figure;
image(result,'CDataMapping','scaled');
colorbar

