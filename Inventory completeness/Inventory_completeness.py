import os
import pandas as pd
import numpy as np
import sys
import glob


def species_accumulation_curve(data, num_permutations=100):
    num_samples = data.shape[0]
    species_richness = np.zeros((num_permutations, num_samples))

    for i in range(num_permutations):
        np.random.shuffle(data)  # Shuffle the order of samples
        cumulative_species = set()

        for j in range(num_samples):
            # Update set with species present in the sample (where data[j] == 1)
            cumulative_species.update(np.where(data[j] == 1)[0])
            species_richness[i, j] = len(cumulative_species)

    mean_richness = np.mean(species_richness, axis=0)
    std_deviation = np.std(species_richness, axis=0)

    return mean_richness, std_deviation


def process(fn):

    total = pd.read_csv(fn)

    if len(total) < 10:
        return

    # Get unique hexIDs
    hexIDs = total['hexID'].unique()

    # DataFrame to store slopes
    slopes = pd.DataFrame(columns=['slope', 'NumObs', 'NumSpecies'])
    cell = total[total['hexID'] == hexIDs[0]]
    species_counts = cell.groupby('species')['NumObs'].sum().to_dict()
    counts=list(species_counts.values())
    species = list(species_counts.keys())
    print('number of species:', len(species))
    print('number of observations:', np.sum(counts))
    data = np.zeros((np.sum(counts),len(species_counts)), dtype = np.int32)
    row=0
    for i,spec in enumerate(species):
        j = species.index(spec)
        count = counts[i]
        data[row:row+count,j] = 1
        row += count
    
    richness, std = species_accumulation_curve(data, num_permutations=100)

    x1 = len(richness)
    x2 = int(np.floor(len(richness)*0.9))
    slope = (richness[-1] - richness[x2-1]) / (x1 - x2)

    print(hexIDs[0], slope)

    slope_dict = {'hexID':hexIDs[0], 'slope': slope, 'NumObs': np.sum(counts), 'NumSpecies': len(species)}
    slopes = pd.DataFrame([slope_dict])
    path = "/scratch/negishi/choi794/python_result1/"
    os.makedirs(path, exist_ok=True)
    fid = os.path.basename(fn)
    new_filename = f"{os.path.splitext(fid)[0]}_inventory_completeness.csv"
    output_file = os.path.join(path, new_filename)
    slopes.to_csv(output_file, index=False)


if __name__ == "__main__":
   
    with open('file_list.txt','r') as f:
        files = [line.strip() for line in f.readlines()]
        
        files = files[int(sys.argv[1]):int(sys.argv[2])]

        for fn_ in files:
            fid = os.path.basename(fn_)
            print(fid)
            dataroot = '/scratch/negishi/choi794/all_hexID/hexID/'
            fn = os.path.join(dataroot,fid)
            print(f'Start processing {fn}')
            process(fn)
