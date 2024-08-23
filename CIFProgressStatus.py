# import packages used in this script
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


def update_progress_status_meta(progress_dict, indicator_id):
    meta_file = indicator_id + '.md'
    filepath = os.path.join('meta', meta_file)
    meta = yamlmd.read_yamlmd(filepath)
    meta[0].update(progress_dict)
    write_meta_md(meta, indicator_id)


def diff_note(old, new):
    now = dt.now().strftime("%d/%m/%Y %H:%M:%S")
    return 'progress status has changed from ' + old + ' to ' + new + ' (' + now + ')'


def write_yaml(output, filename):
    filepath = os.path.join(filename)
    with open(filepath, 'w') as file:
        yaml.dump(output, file)


def update_progress_diff(diff):
    #try: 
        filepath = os.path.join('progress_diff.yml')
        with open(filepath, 'r') as stream:
            diff_file = yaml.safe_load(stream)
            if diff_file is None:
                diff_file = {}
        diff_file.update(diff)
        with open(filepath, 'w') as file:
            outputs = yaml.dump(diff_file, file)
    #except:
        #print("An error has been found for ",indicator_ids)


def update_progress_status(indicator_ids):
    all_progress_statuses = {}
    progress_diff = {}

    for ind_id in indicator_ids:

        # print(ind_id)

        # Get data + metadata for calculation
        indicator = merge_indicator(ind_id)
        if indicator is not None:

            # get the old progress measure
            if indicator['meta'].get('progress_status'):
                old_pm = indicator['meta'].get('progress_status')
            else:
                old_pm = None

            # Run data + metadata through calculation to get progress
            progress = pm.progress_measure(indicator)
            all_progress_statuses[ind_id] = progress

            if progress is not None:

                # check if the newly calculate progress measure is different than the old one
                if old_pm and progress != old_pm:
                    progress_diff[ind_id] = diff_note(old_pm, progress)

                # Update progress status field in meta
                progress_dict = {'progress_status': progress}
                # Uncomment to update metadata files
                update_progress_status_meta(progress_dict, ind_id)

    # return the progress differences
    return progress_diff


def get_goal_progress(indicator_ids):
    scores = {}

    for ind_id in indicator_ids:
        indicator = merge_indicator(ind_id)
        score = pm.get_indicator_score(indicator)
        scores[ind_id] = score

    # manual_input = {'6-1-1': 5, '5-2-1': 0.59005, '6-3-1': 0.21765, '7-2-1': 3.042379, '8-6-1': -3.27486, '9-1-1': 5, '14-2-1': 3.186908, '15-2-1': 4.453995}
    # scores.update(manual_input)

    filepath = os.path.join('indicator_scores.yml')
    with open(filepath, 'w') as file:
        outputs = yaml.dump(scores, file)

    return scores


def get_progress_measure_results(indicator_ids):
    """
    Return a dictionary containing the progress measure results and calculation components
    for each indicator in the input list (indicator_ids).
    """
    progress_results = {}

    for ind_id in indicator_ids:
        # print(ind_id)
        indicator = merge_indicator(ind_id)

        meta = indicator['meta']
        data = indicator['data']

        progress_result = pm.measure_indicator_progress(data, meta)

        if progress_result is None:
            progress_results[ind_id] = {'progress_status': 'not_available'}
        elif "config" in progress_result.keys():
            progress_results[ind_id] = {'base_year': str(progress_result['config']['base_year']),
                                        'current_year': str(progress_result['config']['current_year']),
                                        'base_value': str(progress_result['config']['base_value']),
                                        'current_value': str(progress_result['config']['current_value']),
                                        'direction': progress_result['config']['direction'],
                                        'target_year': str(progress_result['config']['target_year']),
                                        'target': str(progress_result['config']['target']),
                                        'progress_calculation_value': str(progress_result['value']),
                                        'progress_status': progress_result['progress_status'],
                                        }
        else:
            progress_results[ind_id] = progress_result
            
    return progress_results


def get_scores(progress_results):
    """
    Given a dictionary of progress measure results (see get_progress_measure_results),
    return another dictionary containing the score for each indicator. 
    """
    scores = {}
    for ind_id in progress_results:
        # print(ind_id)
        value = progress_results[ind_id].get('progress_calculation_value')
        if value is None:
            score = None
        elif value == 'None':
            score = 5
        else:
            value = float(value)
            target = progress_results[ind_id].get('target')
            if target == 'None':
                target = None
            elif target is not None:
                target = float(target)
            score = pm.score_calculation(value, target)
        scores[ind_id] = score

    # manual_input = {'6-1-1': 5, '5-2-1': 0.59005, '6-3-1': 0.21765, '7-2-1': 3.042379, '8-6-1': -3.27486, '9-1-1': 5, '14-2-1': 3.186908, '15-2-1': 4.453995}
    # scores.update(manual_input)

    return scores


def update_progress_status2(progress_results):
    """
    Compare progress statuses in the input dictionary (progress_results) and in metadata files. 
    If the progress_status field in the metadata is different from the progress_status in the
    input dictionary, update the metadata with the progress_status value from the input.
    Return a dictionary containing a string descriptions of progress status changes.
    """
    diffs = {}
    for ind_id in progress_results:
        # Get old progress status from metadata file
        meta = read_meta_md(ind_id)
        old_status = meta.get('progress_status')
        # Get calculated progress status from input dictionary
        new_status = progress_results[ind_id]['progress_status']
        # Check if the newly calculated progress measure is different from the old one
        if old_status != new_status:
            diffs[ind_id] = diff_note(old_status, new_status)
            # Update progress status field in metadata
            update_progress_status_meta({'progress_status': new_status}, ind_id)
    return diffs


if __name__ == "__main__":
    # get all indicator ids from listed data files
    indicator_ids = get_indicator_ids()

    # calculate progress measure values for all indicators
    progress_results = get_progress_measure_results(indicator_ids)
    write_yaml(progress_results, 'indicator_calculation_components.yml')

    # calculate indicator scores
    scores = get_scores(progress_results)
    write_yaml(scores, 'indicator_scores.yml')

    # Update metadata with new progress statuses
    diffs = update_progress_status2(progress_results)
    # if there have been changes to any progress measure, update the difference file
    if diffs:
        update_progress_diff(diffs)