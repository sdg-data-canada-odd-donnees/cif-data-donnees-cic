import yamlmd
import os
from CIFProgressStatus import get_indicator_ids
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


ind_ids = get_indicator_ids()
# ind_ids = ['11-2-1']

for ind in ind_ids:
    delete_progress_status(ind)

