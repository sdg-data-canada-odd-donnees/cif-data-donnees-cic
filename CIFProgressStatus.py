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


def update_progress_status(progress_dict, indicator_id):
    meta_file = indicator_id + '.md'
    filepath = os.path.join('meta', meta_file)
    meta = yamlmd.read_yamlmd(filepath)
    meta[0].update(progress_dict)
    write_meta_md(meta, indicator_id)


indicator_ids = get_indicator_ids()
# TEMP ----
# indicator_ids = ['16-2-1']
# ----

for ind_id in indicator_ids:
    # Uncomment to turn on ALL indicator calculation
    # turn_on_progress_calc(ind_id)

    # Get data + metadata for calculation
    indicator = merge_indicator(ind_id)
    if indicator is not None:
        # Run data + metadata through calculation to get progress
        progress = pm.measure_indicator_progress(indicator)

        if progress is None:
            progress = 'not_available'

        print(ind_id + ': ' + progress)

        # Update progress status field in meta
        progress_dict = {'progress_status': progress}
        update_progress_status(progress_dict, ind_id)
        print(progress_dict)

# individal calculations result ----
# test_ind = merge_indicator('14-2-1')
# test_data = pm.data_progress_measure(test_ind['data'])
# print(test_data)
# val1 = pm.growth_calculation(98.33, 95.60, 2020, 2016)
# print(val1)
# print(pm.measure_indicator_progress(test_ind))