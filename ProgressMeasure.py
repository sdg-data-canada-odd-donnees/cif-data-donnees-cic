def measure_indicator_progress(data, config):
    """Sets up all needed parameters and data for progress calculation, determines methodology for calculation,
    and returns progress measure as an output.

    Args:
        data:
        config:
    Returns:
        output: str. A string indicating the progress measurement for the indicator.
    """

    # data = indicator['data']    # get indicator data
    # config = indicator['meta']  # get configurations

    if not config.get('auto_progress_calculation'):
        # If auto_progress_calculation is turned off, take progress_status from metadata if it exists or not_available otherwise.
        return {'progress_status': config.get('progress_status', 'not_available')}

    else:
        config = get_progress_calculation_options(config)
        
        # get relevant data to calculate progress (aggregate/total line only)
        data = data_progress_measure(data, config)
        if data is None:
            return None

        # get years that exist in the data & set current year
        # to be the most recent year that exists in data
        years = data["Year"]
        config['current_year'] = years.max()
        config['current_value'] = data.Value[data.Year == config['current_year']].values[0]

        # check if the base year input exists in the data
        if config['base_year'] not in years.values:
            # return None if the base year input is in the future of the most recently available data
            if config['base_year'] > years.max():
                return None

            # if base year is not in available data and not in the future,
            # assign it to be the minimum existing year past the base year given
            config['base_year'] = years[years > config['base_year']].min()

        config['base_value'] = data.Value[data.Year == config['base_year']].values[0]

        # return None if there is not enough data to calculate progress (must be at least 2 data points)
        if config['current_year'] - config['base_year'] < 1:
            return None

        # determine which methodology to run
        # if no target exists, run methodology for qualitative target.
        # else run methodology for quantitative target.
        if config['target'] is None:
            # update progress thresholds for qualitative target
            config = update_progress_thresholds(config, method=1)
            # do progress calculation according to methodology for qualitative target
            value = methodology_1(data=data, config=config)

        else:
            # update progress thresholds for quantitative target
            config = update_progress_thresholds(config, method=2)
            # do progress calculation according to methodology for quantitative target
            value = methodology_2(data=data, config=config)

        return {'value': value, 'progress_status': get_progress_status(value, config), 'config': config}


def get_progress_status(value, config):

    x = float(config['high'])
    y = float(config['med'])
    z = float(config['low'])

    # compare value to progress thresholds to return progress measure
    if config['target'] is None:
        if value >= x:
            return "substantial_progress"
        elif y <= value < x:
            return "moderate_progress"
        elif z <= value < y:
            return "negligible_progress"
        elif value < z:
            return "deterioration"
        else:
            return None

    else:
        if value == None:
            return "target_achieved"
        elif value >= x:
            return "substantial_progress"
        elif y <= value < x:
            return "moderate_progress"
        elif z <= value < y:
            return "negligible_progress"
        elif value < z:
            return "deterioration"
        else:
            return None


def config_defaults(config):
    """Set progress calculation defaults and update them if any user inputs exist.
    Args:
        config: dict. Indicator configurations passed as a dictionary.
    Returns:
        dict: Dictionary of updated configurations.
    """

    # set default options for progress measurement
    defaults = default_progress_calc_options()
    # update the defaults with any user configured inputs
    defaults.update(config)

    # if target is 0, set to 0.001 (avoids dividing by 0 in calculation)
    if defaults['target'] == 0:
        defaults['target'] = 0.001

    return defaults


def default_progress_calc_options():
    """Provide default inputs for calculating progress."""
    return (
        {
            'base_year': 2015,
            'target_year': 2030,
            'direction': 'negative',
            'target': None,
            'progress_thresholds': {}
        }
    )


def get_progress_calculation_options(metadata):
    """
    Get the progress_calculation_options from metadata.
    """
    if 'progress_calculation_options' in metadata.keys():
        return config_defaults(metadata['progress_calculation_options'][0])
    else:
        return default_progress_calc_options()


def update_progress_thresholds(config, method):
    """Checks for configured progress thresholds or updates thresholds based on methodology.
    Args:
        config: dict. Progress calculation inputs for indicator.
        method: int. Indicates which methodology is being used. Either 1 (for qualitative targets) or 2 (for
                quantitative targets).
    Returns:
        dict: Dictionary of updated inputs for calculation.
    """

    # if progress threshold inputs exist and are not empty, assign user input value as thresholds
    # otherwise if progress threshold inputs are empty, use defaults
    if ('progress_thresholds' in config.keys()) & (bool(config['progress_thresholds'])):
        progress_thresholds = config['progress_thresholds']
    elif method == 1:
        progress_thresholds = {'high': 0.015, 'med': 0.005, 'low': 0}
    elif method == 2:
        progress_thresholds = {'high': 0.95, 'med': 0.6, 'low': 0}
    else:
        progress_thresholds = {}

    # update inputs with thresholds
    config.update(progress_thresholds)

    return config


def data_progress_measure(data, config):
    """Checks and filters data for indicator for which progress is being calculated.

    If the Year column in data contains more than 4 characters (standard year format), takes the first 4 characters.
    If data contains disaggregation columns, take only the total line data.
    Removes any NA values.
    Checks that there is enough data to calculate progress.

    Args:
        data: DataFrame. Indicator data for which progress is being calculated.
    Returns:
        DataFrame: Data in valid format for calculating progress.
    """

    # check if the year value contains more than 4 digits (indicating a range of years)
    if (data['Year'].astype(str).str.len() > 4).any():
        # take the first year in the range
        data['Year'] = data['Year'].astype(str).str.slice(0, 4).astype(int)

    # get just the total line values from data
    cols = data.columns
    if len(cols) > 2:
        # Data has disaggregations, find headline data
        # If units or series are given, select desired unit/series to use for progress measure calculation
        if ("Units" in cols) and ("unit" in config.keys()):
            data = data.loc[data["Units"] == config["unit"]]
        if ("Series" in cols) and ("series" in config.keys()):
            data = data.loc[data["Series"] == config["series"]]
        # Find rows with all empty values (excluding Year, Units, Series, and Value columns)
        data = data[data.loc[:, ~cols.isin(["Year", "Units", "Series", "Value"])].isna().all("columns")]
        data = data.iloc[:, [0, -1]]

    # remove any NA values from data
    data = data[data["Value"].notna()]

    # returns None if no rows in data (no total line to calculate progress)
    if data.shape[0] < 1:
        return None

    return data


def growth_calculation(val1, val2, t1, t2):
    """Calculate cumulative annual growth rate with required arguments.

    Args:
        val1: float. Current value.
        val2: float. Value from base year.
        t1: float. Current year.
        t2: float. Base year.
    Returns:
        float: Growth value.
    """

    return ((val1 / val2) ** (1 / (t1 - t2))) - 1


def methodology_1(data, config):
    """Calculate growth using progress measurement methodology 1 (no target value).

    Use configuration options to get the current and base value from indicator data and use to calculate growth.
    Compare growth to progress thresholds to return a progress measurement.

    Args:
        data: DataFrame. Indicator data for which progress is being calculated.
        config: dict. Configurations for indicator for which progress is being calculated.
    Returns:
        str: Progress measure.
    """

    direction = str(config['direction'])
    t = float(config['current_year'])
    t_0 = float(config['base_year'])
    x = float(config['high'])
    y = float(config['med'])
    z = float(config['low'])

    # get current value from data
    current_value = data.Value[data.Year == t].values[0]
    # get value from base year from data
    base_value = data.Value[data.Year == t_0].values[0]
    # calculate growth
    cagr_o = growth_calculation(current_value, base_value, t, t_0)

    # use negative growth value if desired direction of progress is negative
    if direction == "negative":
        cagr_o = -1 * cagr_o

    return cagr_o


def methodology_2(data, config):
    """Calculate growth using progress measurement methodology 2 (given target value).

    Check if target has already been achieved.
    Use configuration options to get the current and base value from indicator data and use to calculate growth ratio.
    Compare growth ratio to progress thresholds to return a progress measurement.

    Args:
        data: DataFrame. Indicator data for which progress is being calculated.
        config: dict. Configurations for indicator for which progress is being calculated.
    Returns:
        str: Progress measure.
    """

    direction = str(config['direction'])
    t = float(config['current_year'])
    t_0 = float(config['base_year'])
    target = float(config['target'])
    t_tao = float(config['target_year'])

    # get current value from data
    current_value = data.Value[data.Year == t].values[0]
    # get base value from data
    base_value = data.Value[data.Year == t_0].values[0]

    # check if the target is achieved
    if (direction == "negative" and current_value <= target) or (direction == "positive" and current_value >= target):
        # None value indicates that target has been achieved
        return None 

    # calculate observed growth
    cagr_o = growth_calculation(current_value, base_value, t, t_0)
    # calculate theoretical growth
    cagr_r = growth_calculation(target, base_value, t_tao, t_0)
    # calculating growth ratio
    ratio = cagr_o / cagr_r

    return ratio


def progress_measure(indicator):

    if indicator is None:
        return None

    data = indicator['data']  # get indicator data
    config = indicator['meta']  # get configurations

    progress_calc = measure_indicator_progress(data, config)

    if progress_calc is None:
        return None

    value = progress_calc['value']
    config = progress_calc['config']

    if value is None:
        output = "target_achieved"
    else:
        output = get_progress_status(value, config)

    return output


def score_calculation(value, target):

    if value is None:
        return None
    elif target is None:
        if value > 0:
            return min(value * 250, 5)
        else:
            return max(value * 250, -5)
    elif bool(target):
        if value > 0.6:
            return min((7.1429 * value) - 4.2857, 5)
        else:
            return max((4.1667 * value) - 2.5, -5)


def get_indicator_score(indicator):

    if indicator is None:
        return None

    data = indicator['data']  # get indicator data
    config = indicator['meta']  # get configurations

    progress = measure_indicator_progress(data, config)

    if progress is None:
        return None

    config = progress['config']
    value = progress['value']
    target = config['target']

    if value is None:
        return 5

    if not bool(target):
        target = None
    else:
        target = float(target)

    return float(score_calculation(value, target))
