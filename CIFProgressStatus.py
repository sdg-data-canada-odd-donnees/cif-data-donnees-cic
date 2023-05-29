import statistics
import yamlmd
import os
import pandas as pd
import ProgressMeasure as pm
import yaml
from datetime import datetime as dt


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


def update_progress_status_meta(progress_dict, indicator_id):
    meta_file = indicator_id + '.md'
    filepath = os.path.join('meta', meta_file)
    meta = yamlmd.read_yamlmd(filepath)
    meta[0].update(progress_dict)
    write_meta_md(meta, indicator_id)


def diff_note(old, new):
    now = dt.now().strftime("%d/%m/%Y %H:%M:%S")
    return 'progress status has changed from ' + old + ' to ' + new + ' (' + now + ')'


def update_progress_diff(diff):
    filepath = os.path.join('progress_diff.yml')
    with open(filepath, 'r') as stream:
        diff_file = yaml.safe_load(stream)
    diff_file.update(diff)
    with open(filepath, 'w') as file:
        outputs = yaml.dump(diff_file, file)


def update_progress_status(indicator_ids):
    all_progress_statuses = {}
    progress_diff = {}

    for ind_id in indicator_ids:
        # Uncomment to turn on ALL indicator calculation
        # turn_on_progress_calc(ind_id)

        # Get data + metadata for calculation
        indicator = merge_indicator(ind_id)
        if indicator is not None:

            if indicator['meta'].get('progress_status'):
                old_pm = indicator['meta'].get('progress_status')
            else:
                old_pm = None

            # Run data + metadata through calculation to get progress
            progress = pm.progress_measure(indicator)
            all_progress_statuses[ind_id] = progress

            if progress is not None:

                if old_pm and progress != old_pm:
                    progress_diff[ind_id] = diff_note(old_pm, progress)

                print(ind_id + ': ' + progress)
                # Update progress status field in meta
                progress_dict = {'progress_status': progress}
                # Uncomment to update metadata files
                update_progress_status_meta(progress_dict, ind_id)
    return progress_diff


def get_goal_progress(indicator_ids):

    scores = {}

    for ind_id in indicator_ids:
        # goal = ind_id[0]
        indicator = merge_indicator(ind_id)
        score = pm.get_indicator_score(indicator)
        scores[ind_id] = score
        # print(ind_id + ': ' + str(score))

    manual_input = {'6-1-1': 5, '5-2-1': 0.59005, '6-3-1': 0.21765, '7-2-1': 3.042379, '8-6-1': -3.27486, '9-1-1': 5, '14-2-1': 3.186908, '15-2-1': 4.453995}
    scores.update(manual_input)

    filepath = os.path.join('indicator_scores.yml')
    with open(filepath, 'w') as file:
        outputs = yaml.dump(scores, file)

    return scores


indicator_ids = get_indicator_ids()
# scores = get_goal_progress(indicator_ids)
# diffs = update_progress_status(indicator_ids)
# update_progress_diff(diffs)



# CODE BELOW CALCULATES AVERAGE GOAL SCORE
# goal_scores = {}
# for i in range(1, 18):
#     goal = str(i)
#     goal_scores[goal] = []
#
# for ind_id in indicator_ids:
#     goal = ind_id.split("-")[0]
#     if scores[ind_id] is not None:
#         goal_scores[goal].append(scores[ind_id])
#
# for i in range(1, 18):
#     goal = str(i)
#     score_list = goal_scores[goal]
#     if len(score_list) > 0:
#         score_avg = statistics.mean(score_list)
#         print('goal ' + goal + ': ' + str(score_avg))
#     else:
#         print("non")


# individual calculations result ----
test_ind = merge_indicator('14-2-1')
test_data = pm.data_progress_measure(test_ind['data'])
# print(test_ind['meta'])
# print(test_data)
# test_cagr_o = pm.growth_calculation(30, 103, 2022, 2015)
# print('cagr_o = ' + str(test_cagr_o))
# test_cagr_r = pm.growth_calculation(0.001, 103, 2030, 2015)
# print('cagr_r = ' + str(test_cagr_r))
# test_ratio = test_cagr_o/test_cagr_r
# print('ratio = ' + str(test_ratio))
print(pm.measure_indicator_progress(test_data, test_ind['meta']))
print(pm.progress_measure(test_ind))
print(pm.get_indicator_score(test_ind))