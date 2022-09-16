import yamlmd
import os
import pandas as pd
import ProgressMeasure as pm


def read_meta_md(indicator_id):
    meta_file = indicator_id + '.md'
    filepath = os.path.join('meta', meta_file)
    if not os.path.isfile(filepath):
        return None
    meta_md = yamlmd.read_yamlmd(filepath)
    meta = dict(meta_md[0])
    meta['page_content'] = ''.join(meta_md[1])
    return meta


def write_meta_md(meta, indicator_id):
    meta_file = indicator_id + '.md'
    filepath = os.path.join('meta', meta_file)
    yamlmd.write_yamlmd(meta, filepath)


def read_data(indicator_id):
    data_file = 'indicator_' + indicator_id + '.csv'
    filepath = os.path.join('data', data_file)
    if not os.path.isfile(filepath):
        return None
    data = pd.read_csv(filepath)
    return data


# def progress_dict(indicator_id, progress_measure):
#     # TODO: read progress dict
#     {indicator_id: progress_measure}
#     # TODO: write new progress status to dict


def get_indicator_ids():
    ids = []
    for file in os.listdir('meta'):
        # take just the file name (remove file extension) to get indicator id and add to list
        if file.endswith('.md'):
            id_name = file[:-3]
            ids.append(id_name)
    return ids


def merge_indicator(indicator_id):
    test_meta = read_meta_md(indicator_id)
    test_data = read_data(indicator_id)
    if test_data is None or test_meta is None:
        return None
    indicator = {'meta': test_meta, 'data': test_data}
    return indicator


def turn_on_progress_calc(indicator_id):
    """
    Force turns on progress calculation in meta markdown file for whatever indicator is passed.
    :param indicator_id: id for indicator (e.g. 1-1-1)
    :type indicator_id: str
    """
    meta_file = indicator_id + '.md'
    filepath = os.path.join('meta', meta_file)
    meta = yamlmd.read_yamlmd(filepath)
    if 'auto_progress_calculation' not in meta[0].keys():
        meta[0]['auto_progress_calculation'] = True
    elif not meta[0]['auto_progress_calculation']:
        meta[0]['auto_progress_calculation'] = True
    write_meta_md(meta, indicator_id)

indicator_ids = get_indicator_ids()
# indicator_ids = indicator_ids[0:11] # TEMP
# print(indicator_ids)

progress_dict = {}

for ind_id in indicator_ids:
    # Uncomment to turn on ALL indicator calculation
    # turn_on_progress_calc(ind_id)
    # Get data + metadata for calculation
    indicator = merge_indicator(ind_id)
    if indicator is not None:
        # Run data + metadata through calculation to get progress
        progress = pm.measure_indicator_progress(indicator)
        # TEMP: Return None str for no progress calculation (to be able to concatenate)
        if progress is None:
            progress = 'None'
        print(ind_id + ': ' + progress)
    # progress_dict[ind_id] = progress

print(progress_dict)

# 6.3.1 complex result ----
# TODO: Handle complex outcomes! (Not even sure what to do to fix this)
# test_ind = merge_indicator('6-3-1')
# test_data = pm.data_progress_measure(test_ind['data'])
# print(test_data)
# val = pm.growth_calculation(1.0, -2.7, 2017, 2015)
# print(val)
# print(type(val))