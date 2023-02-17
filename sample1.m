%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   MAIN PROGRAM
%
%   BATCH SOM
%
%   function [t]=batch_SOM
%
%   Dr.V.
%   Revision: 4.8
%
%
%   X Y=mapsize
%   [s]=batch_SOM_eud(I,I_orig,80,100,30,30,1,'rand',[],'range','rough')
%
%   normalizationtype = var, range
%   initializationtype = rand, linear, grad, oldmap
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [s]=batch_SOM_eud(I,I_orig,mid_epoch,final_epoch,X,Y,find_unique,initializationtype,nodes_weight,normalizationtype,tuning)

%rand('seed',sum(clock)*7)
tic
if  isinteger(I)
    I=double(I);
end

%% find unique profiles if needed
if find_unique == 1
    disp('Unique-fying...')
    [unique_profiles,positions_in_original,positions_in_unique]=unique(I,'rows');
    I = unique_profiles;
end

[N_numofrows,dim]=size(I);

%% Convert I data to structure
disp('Structuring...')
I = som_data_struct(I);


if ~isempty(normalizationtype)
    %% Normalization
    disp('Normalizing...')
    I = som_normalize(I,normalizationtype);
else
    disp('NO Normalization...')
end

%final_epoch=input('How long to epoch for?\n');

%% Load mapsize, number of nodes, square only
disp('Map Sizing...')
%load mapsize % this loads rectangular map 100 x 50
%nodes_cartesian_coord = mapsize{num_of_nodes};
nodes_cartesian_coord = make_neural_map(X,Y);

if isempty(nodes_cartesian_coord)
    disp('Map Size invalid. Choose another map size.')
    s = -1;
    return
end

clear mapsize

[N_numofnodes] = size(nodes_cartesian_coord,1);
radius_of_neighborhood = zeros(1,final_epoch);

%% Fine or rough tuning
if strcmpi('fine',tuning) == 1 %fine tuning
    radius_of_neighborhood = 1.2*ones(final_epoch,1);
elseif strcmpi('rough',tuning) == 1 %rough tuning
    radius_of_neighborhood(1) = sqrt(N_numofnodes)+4; % initial radius
    %radius_of_neighborhood(1) = 2;
    load extras radT
    final_radius = radT;

    x=-mid_epoch*log10(radius_of_neighborhood(1))/(log(final_radius/radius_of_neighborhood(1)));
    lamda_N=x/log10(radius_of_neighborhood(1)); % neighborhood exponential factor
    %% Pre-populate radius of neighborhood
    k=1;
    for i=1:final_epoch
        if i <= mid_epoch
            radius_of_neighborhood(i) = radius_of_neighborhood(1)*exp(-i/lamda_N); %version 1
        elseif i > mid_epoch
            radius_of_neighborhood(i) = *exp(-k/(final_epoch-mid_epoch)); %version 1
            k = 1+k;
        end
    end
end


%% Randomize initial map weights
disp('Initializing...')
disp(initializationtype)
dimensions_map=max(nodes_cartesian_coord);
mapX=dimensions_map(1);
mapY=dimensions_map(2);
if strcmpi('rand',initializationtype)
    s = som_randinit(I, 'msize',[mapX mapY]);
    nodes_weight=s.codebook;
elseif strcmpi('linear',initializationtype)
    s = som_lininit(I, 'msize',[mapX mapY]);
    nodes_weight=s.codebook;
elseif strcmpi('grad',initializationtype)
    % % % % % % % gradient_initialization
    s = som_randinit(I, 'msize',[mapX mapY]);
    [nodes_weight]=gradient_initialization(s.codebook,nodes_cartesian_coord);
    s.codebook=nodes_weight;
    % % % % % % % gradient_initialization
elseif strcmpi('oldmap',initializationtype)
    s = som_randinit(I, 'msize',[mapX mapY]);
    s.codebook=nodes_weight;
else
    disp('No such initialization! Try again!')
    s=-1;
    return
end

% % % % % % % sample initialization
%sequence = randperm(N_numofrows);
%which_rows=unidrnd(N_numofrows,mapX*mapY,1);
%s.seq=sequence(1:mapX*mapY);
%.codebook=I.data(sequence(1:mapX*mapY),:);
% % % % % % % sample initialization

% s = som_lininit(I, 'msize',[sqrt(N_numofnodes) sqrt(N_numofnodes)]);

% if isempty(nodes_weight) == 1
%     nodes_weight = s.codebook;
% end

data = I.data; % this data refers to input data stream
mask = s.mask;
Ud = som_unit_dists(s.topol);
Ud = Ud.^2;
bmus = zeros(1,N_numofrows);
ddists = zeros(N_numofrows,1);

%% Start training
WW = 2*data';

for epoch=1:final_epoch
    tic

    fprintf('Sorting Epoch %d \n', epoch)
    %W = mask*ones(1,N_numofrows).*(~isnan(data))';
    blen = min(N_numofnodes,N_numofrows);
    % ggg = ormaliz_new(nodes_weight);
    i0 = 0;
    aaa = sum(nodes_weight.^2,2);
    while i0+1 <= N_numofrows,

        inds = (i0+1):min(N_numofrows,i0+blen);

        %dist = (nodes_weight.^2)*W(:,inds)-nodes_weight*WW(:,inds);%+ones(N_numofnodes,1)*mask'*data'.^2;

        col = min(N_numofrows,i0+blen)-i0;
        dist = repmat(aaa,1,col)-nodes_weight*WW(:,inds);

        %dist = normaliz_new(nodes_weight)*WW(:,inds);
            %col = min(N_numofrows,i0+blen)-i0;
            %dist = ones(size(nodes_weight,1),col)-normaliz_new(nodes_weight)*WW(:,inds);

        % %                        W=mask*ones(1,max(inds)-i0);
        % %                        WW=2*diag(mask)*data(inds,:)';
        i0 = i0+blen;

        [ddists(inds), bmus(inds)] = min(dist);

%         pause(4)
        %A = rand(500);eig(A); %40
        %A = eig(rand(1000)); %130
        %clear A
        %A = rand(1000);eig(A); %130
    end

    neigh = 'gaussian';

    switch  neigh
        case 'bubble',   H = (Ud<=radius_of_neighborhood(epoch));
        case 'gaussian', H = exp(-Ud/(2*radius_of_neighborhood(epoch).^2));
        case 'cutgauss', H = exp(-Ud/(2*radius_of_neighborhood(epoch))) .* (Ud<=radius_of_neighborhood(epoch));
        case 'ep',       H = (1-Ud/radius_of_neighborhood(epoch)) .* (Ud<=radius_of_neighborhood(epoch));
    end

    P = sparse(bmus,1:N_numofrows,1,N_numofnodes,N_numofrows);
    AA = H*(P*data); % top
    %BB= H*(P*(~isnan(data))); % bottom
    BB = H*(P*(ones(size(data)))); % bottom
    %nonzero = find(BB>0);
    %nodes_weight(nonzero) = AA(nonzero)./BB(nonzero);
    nodes_weight =A A./BB;

    toc
end %% End training

%% Denormalization of input data
II = som_denormalize(I, 'remove'); % storing input data into struct
s.I = II.data;
s.neigh = neigh;
s.codebook = nodes_weight;
s.nodes_cartesian_coord = int32(nodes_cartesian_coord);

%% Som Quality function
%        [qe,te] = som_quality(s,I)
%        [color] = som_kmeanscolor(s,10,som_colorcode(s,'hsv'),'enhanced'); % has to use normalized data
%        som_show(s,'color',color);

%% Denormalization of trained data
s = som_denormalize(s, 'remove');

%% Zero out all small values
s.codebook(s.codebook<=1e-4)=0;


%% Remove fields not needed
s = rmfield(s, {'name' 'labels' 'comp_names' 'comp_norm' 'trainhist'});
s.mapX = mapX;
s.mapY = mapY;

%% Find land/swim values
lamda = som_quality_my(s,0,1);
s.landorswim = lamda';

%% Find user2node
threshold = 0.0; needs_normalize = 0; metric = 'blockdotproduct';
if find_unique == 1
    [user2node_unique] = find_nodes_for_each_user(s,threshold,needs_normalize,[],metric);
    s.user2node = user2node_unique(positions_in_unique,:);
    s.positions_in_unique = positions_in_unique;
    %[user2nodes_unique] = find_nodes_for_each_user_ver3(s,threshold,needs_normalize,[],metric);
    %s.user2nodes = user2nodes_unique(positions_in_unique,:);
else
    [user2node] = find_nodes_for_each_user(s,threshold,needs_normalize,I_orig,metric);
    s.user2node = user2node;
end

s.bmus = bmus';
s.threshold = threshold;
s.date = datestr(now);

% Map INPUT users to SOM;
% [data_reverse_map] = consumer_data_mapping(s,0); % 0 means don't plot!!
% s.data_reverse_map = data_reverse_map;

toc
