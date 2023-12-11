%this script with format the update teeth data into the form we would like 

%Two files with data:
teethBWtrain500matrix20210622 = readtable("./gladysvale/teeth_BW_train_500_matrix_20210622.csv")
teethBWtrain500matrix20210622 = renamevars(teethBWtrain500matrix20210622,["Var1"],["image"])


referencefile20210622 = readtable("/Users/gregorymatthews/Dropbox/gladysvale/reference_file_20210622.csv")

%teethBWtrain500matrix20210622 is a file with 7070
%rows and 501 columns. The first column is the image number and the other
%500 rows are sample x and y coordinates. Each pair of rows (so 1:2,
%3:4,...) are the x,y coords for an image. (3535 images total)

%referencefile20210622 is a file with information about 3909 images. thew
%columns are variable numbers and it contains the same image numbers used
%in teethBWtrain...

%% Subset referencefile so that it only has the images in teethBWtain

%this gives the unique image names in teethBWtrain 
image_unique = unique(teethBWtrain500matrix20210622.image);

%1 is the image in referencefile is in teethBWtrain and 0 otherwise
%this has 3 more observations than we have in teethBWtrain, duplicates?
ref_in_teeth = ismember(referencefile20210622.image, image_unique);

%subset referencefile to only have rows with 1 in ref_in_teeth
%this has 303 more observations than we have in teethBWtrain
%these are duplicates and we can remove them for our analysis 
reference_file = referencefile20210622(ref_in_teeth,:);

%get the index to the unique images, we will just use the first occurance
%of duplicates 
[uniqueA i j] = unique(reference_file.image,'first');
indexToUnique = find(ismember(1:numel(reference_file.image),i));

%final subset to get the reference file we want
teeth_ref = reference_file(indexToUnique,:); 

%extract the variables from teeth_ref that we want 
teeth_ref = teeth_ref(:, {'family','tribe','genus','species','type','image','complete','corrupt'});

%save teeth_ref
%save('teeth_ref.mat','teeth_ref')

%% format the teethBWtrain dataset into a set of 3535 2x500 matrices
%split into 2x500x3535 multidim array 

%get the number of rows and cols
n_rows = size(teethBWtrain500matrix20210622,1);
n_cols = size(teethBWtrain500matrix20210622,2);

%blank array storage for matric for each image
teeth_data = zeros(2,100,n_rows/2);

%loop to assign each tooth's coordinates to the appropriate spot in the
%array
for i=1:2:n_rows
    %get index for the image we are on 
    j = find((1:2:n_rows)==i);
    
    %assign matrix for image j 
     X = teethBWtrain500matrix20210622{i:(i+1), 2:n_cols};
    
    %resample so all of the curves have 100 points
    teeth_data(:,:,j) = ReSampleCurve(X,100);
end

% get image ids in order 
teeth_data_ids = teethBWtrain500matrix20210622.image(1:2:n_rows);

%% sort so that the images in teeth_data are in the same order as the variables in teeth_ref

% get the location index in teeth_ref.image for each id in teeth_data
[temp,order] = ismember(teeth_data_ids,teeth_ref.image);

%sort teeth_ref so it is in the same order as the images in teeth_data
teeth_ref = teeth_ref(order,:);

% save the teeth_data and teeth_ref in one .dat file
cd ./full_shape_classification_SRVF/data
save('teeth_data_20210622.mat')
save('teeth_ref')
save('teeth_data')


teeth_ref.type = string(teeth_ref.type)

%start with LM1
%Find overall mean 
%LM1
for toothtype = ["LM1","LM2","LM3","UM1","UM2","UM3"]
    disp(toothtype)
    teeth = teeth_data(:,:,teeth_ref.type == toothtype);
    ref = teeth_ref(teeth_ref.type==toothtype,:);
    
    %compute the overall mean
    %remember to run  mex DynamicProgrammingQ.c before finding the mean
    cd ./full_shape_classification_SRVF/code/matlab
    mean = FindElasticMean(teeth)
    
    %Save data here
    cd ./full_shape_classification_SRVF/data/means
    %Filename first then the object
    save(strcat("mean_",toothtype,"_overall"),"mean")

    for tribe = ["Alcelaphini","Antilopini","Bovini","Hippotragini","Neotragini","Reduncini","Tragelaphini"]
        teeth = teeth_data(:,:,teeth_ref.type == toothtype & teeth_ref.tribe == tribe);
        ref = teeth_ref(teeth_ref.type == toothtype & teeth_ref.tribe == tribe,:);
    
        %compute the overall mean
        %remember to run  mex DynamicProgrammingQ.c before finding the mean
        cd ./full_shape_classification_SRVF/code/matlab
        mean = FindElasticMean(teeth)
    
        %Save data here
        cd ./full_shape_classification_SRVF/data/means
        %Filename first then the object
        save(strcat("mean_",toothtype,"_",tribe),"mean")
    end
end


%Project on the means that have already been found
for toothtype = ["LM1","LM2","LM3","UM1","UM2","UM3"]
    teeth = teeth_data(:,:,teeth_ref.type == toothtype);
    ref = teeth_ref(teeth_ref.type==toothtype,:);
    cd ./full_shape_classification_SRVF/data/means
    load(strcat("mean_",toothtype,"_overall"))
    
    %FindTangentFeatures(mu,q,numPCs)
    cd ./full_shape_classification_SRVF/code/matlab
    sz = size(teeth)
    q = teeth
        for j=1:sz(3)
        q(:,:,j) = curve_to_q(teeth(:,:,j));
        end
    [VV,PC] = FindTangentFeatures(mean,q,min(sz(3) - 1,30));
    %csvwrite(filename,M) writes matrix M to file filename as comma-separated values.
    
    cd ./full_shape_classification_SRVF/data/fulldata/
    writetable(ref, strcat(toothtype,"_train_reference.csv"))
    csvwrite(strcat(toothtype,"_train_overall.csv"), VV)
    csvwrite(strcat(toothtype,"_train_overall_PC.csv"), PC)

    i = 0
    out_all = repmat(0,[sz(3) 2*sz(2), 7])
    out_pc = repmat(0,[sz(3) min(sz(3) - 1,30), 7])
    for tribe = ["Alcelaphini","Antilopini","Bovini","Hippotragini","Neotragini","Reduncini","Tragelaphini"]
        i = i + 1
        disp(i)
        teeth = teeth_data(:,:,teeth_ref.type == toothtype);
        ref = teeth_ref(teeth_ref.type == toothtype ,:);

        cd ./full_shape_classification_SRVF/data/means
        load(strcat("mean_",toothtype,"_",tribe))
    
        %FindTangentFeatures(mu,q,numPCs)
        cd ./full_shape_classification_SRVF/code/matlab
        sz = size(teeth)
        q = teeth
        for j=1:sz(3)
        q(:,:,j) = curve_to_q(teeth(:,:,j));
        end
        [VV,PC] = FindTangentFeatures(mean,q,min(sz(3) - 1,30))
        
        out_all(:,:,i) = VV
        out_pc(:,:,i) = PC
    end

    %concatenate all of them
    train_individual = horzcat(out_all(:,:,1), ...
        out_all(:,:,2), ...
        out_all(:,:,3), ...
        out_all(:,:,4), ...
        out_all(:,:,5), ...
        out_all(:,:,6), ...
        out_all(:,:,7))

    cd ./full_shape_classification_SRVF/data/fulldata 
    csvwrite(strcat(toothtype,"_train_individual.csv"), train_individual)
    

    %Concatenate all of them
    train_individual_PC = horzcat(out_all(:,:,1), ...
        out_pc(:,:,2), ...
        out_pc(:,:,3), ...
        out_pc(:,:,4), ...
        out_pc(:,:,5), ...
        out_pc(:,:,6), ...
        out_pc(:,:,7))

    csvwrite(strcat(toothtype,"_train_individual_PC.csv"), train_individual_PC)

end










