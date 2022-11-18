import yamlmd
import os
# from CIFProgressStatus import get_indicator_ids
from CIFProgressStatus import write_meta_md


def delete_progress_status(indicator_id):
    meta_file = indicator_id + '.md'
    filepath = os.path.join('meta', meta_file)
    meta = yamlmd.read_yamlmd(filepath)
    print(meta)
    if 'progress_status' in meta[0].keys():
        del meta[0]['progress_status']
        print(meta)
    # uncomment if need to delete in metadata files
    # write_meta_md(meta, indicator_id)


def change_page_content(indicator_id, content):
    """
    Change first line of page markdown content of any indicator passed to it.
    WARNING: Overrides old page content!
    :param indicator_id: id for indicator (e.g. 1-1-1)
    :type indicator_id: str
    :param content: string with new page content
    :type indicator_id: str
    """
    meta_file = indicator_id + '.md'
    filepath = os.path.join('meta', meta_file)
    meta = yamlmd.read_yamlmd(filepath)
    meta[1][0] = content
    write_meta_md(meta, indicator_id)


# ind_ids = get_indicator_ids()
ind_ids = ['4-1-1', '5-1-1', '5-1-2', '5-3-1', '8-2-1', '11-1-1', '11-4-1', '11-5-1', '12-2-1', '12-3-1', '16-7-1', '15-1-1']
# ind_ids = ['15-1-1']

for ind in ind_ids:
    change_page_content(ind, content='<i>This indicator does not display a progress status as there is only one data point currently available.</i>')

