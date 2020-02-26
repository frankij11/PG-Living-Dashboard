#Get data
from sodapy import Socrata
import pandas as pd
import difflib
pd.options.display.max_columns = 500


url_md = "opendata.maryland.gov"
#id_md = "pwpp-tk9r"
id_md ="ed4q-f8tm"
app_id = "fuixaI8OmedLW38WcI00DnoCn"

select_statement = '''
    mdp_street_address_mdp_field_address as address,
    mdp_street_address_zip_code_mdp_field_zipcode as zipcode,
    mdp_street_address_city_mdp_field_city as city,
    subdivision_code_mdp_field_subdivsn_sdat_field_37 as neighborhood,
    record_key_owner_occupancy_code_mdp_field_ooi_sdat_field_6 as owner_type,
    sales_segment_1_consideration_mdp_field_considr1_sdat_field_90 as price,
    sales_segment_2_consideration_sdat_field_110 as price2,
    sales_segment_1_transfer_date_yyyy_mm_dd_mdp_field_tradate_sdat_field_89 as date,
    sales_segment_2_transfer_date_yyyy_mm_dd_sdat_field_109 as date2,
    sales_segment_1_grantor_name_mdp_field_grntnam1_sdat_field_80 as seller,
    c_a_m_a_system_data_year_built_yyyy_mdp_field_yearblt_sdat_field_235 as year_built,
    c_a_m_a_system_data_dwelling_grade_code_and_description_mdp_field_strugrad_strudesc_sdat_field_230 as grade,
    current_assessment_year_total_assessment_sdat_field_172 as tax_assessment,
    sales_segment_3_how_conveyed_ind_sdat_field_127 as sales_type_orig,
    c_a_m_a_system_data_structure_area_sq_ft_mdp_field_sqftstrc_sdat_field_241 as living_area,
    c_a_m_a_system_data_land_area_mdp_field_landarea_sdat_field_242 as land_area,
    c_a_m_a_system_data_land_unit_of_measure_mdp_field_luom_sdat_field_243 as land_area_measure,
    land_use_code_mdp_field_lu_desclu_sdat_field_50 as land_use,
    additional_c_a_m_a_data_dwelling_construction_code_mdp_field_strucnst_sdat_field_263 as constr_code,
    additional_c_a_m_a_data_building_style_code_and_description_mdp_field_strustyl_descstyl_sdat_field_264 as bld_code_detail,
    additional_c_a_m_a_data_dwelling_type_mdp_field_strubldg_sdat_field_265 as bld_code,
    latitude_longitude as location,
    mdp_latitude_mdp_field_digycord_converted_to_wgs84 as lat,
    mdp_longitude_mdp_field_digxcord_converted_to_wgs84 as lon
'''
def clean_addresses(addresses):
    props_str = addresses.upper()
    props_str = props_str.replace("STREET", "ST")
    props_str = props_str.replace("AVENUE", "AVE")
    props_str = props_str.replace("DRIVE", "DR")
    props_str = props_str.replace("LN", "LANE")
    props_str = props_str.replace("TERRACE", "TER")
    props_str = props_str.replace("COURT", "CT")
    props_str = props_str.replace(" RD", " ROAD")
    props_str = props_str.replace("TRAIL", "TRL")
    props_str = props_str.replace("PLACE", "PL")
    props_str = props_str.replace("BOULEVARD", "BLVD")
    
    return props_str



def property_metadata(address, save_csv = False, select = select_statement):
    '''
        Query SDAT
        Return dictionary with metadata
    '''
    client = Socrata(url_md, app_id)
    address = clean_addresses(address)
    results = client.get(id_md, 
                     select =select,
                     where ="mdp_street_address_mdp_field_address='{}'".format(address))
    
    df = pd.DataFrame(results)
    closest = difflib.get_close_matches(address.upper(), df.address, n=1)
    #print('Closest Property:', closest[0])
    df = df[df.address == closest[0]]
    #df['lon'] = df.location.apply(lambda x: x['coordinates'][0])
    #df['lat'] = df.location.apply(lambda x: x['coordinates'][1])
    client.close()
    if save_csv:
            sdat = pd.read_csv('sdat_properties.csv')
            #sdat = pd.concat([sdat, df], ignore_index=True)
            result = pd.concat([sdat, df])
            result.to_csv('sdat_properties.csv', index=False)
    return df            

def miles_to_meter(miles):
    return float(miles /0.000621371)

def get_params(df, miles = 1):
    d = dict()
    d['miles'] = miles_to_meter(miles)
    for col in df.columns:
        d[col] = df[col].iloc[0]
    return d

def get_query(df, miles =1):
    q_params = get_params(df, miles)
    query = 'SELECT ' + select_statement +  '''
    
    WHERE  
        sales_segment_1_consideration_mdp_field_considr1_sdat_field_90 > 0 AND
        sales_segment_2_consideration_sdat_field_110 > 0 AND
        within_circle(latitude_longitude, {lat}, {lon}, {miles})


    LIMIT 10000
    '''.format(**q_params)
    return query

def get_comps_sdat(address, miles=1, save_csv=False):
    meta_df = property_metadata(address)
    query = get_query(meta_df, miles)
    client = Socrata(url_md, app_id)
    response = client.get(id_md, query = query)
    df = pd.DataFrame(response)
    num = []
    non_num = []
    for col in df.columns:
        try:
            df[col] = pd.to_numeric(df[col])
            num.append(col)
        except:
            non_num.append(col)

    df['id'] = meta_df.address.iloc[0]
    df.date = pd.to_datetime( df['date'])
    df.date2= pd.to_datetime(df['date2'])
    df['basement'] = df['style_code'].apply(lambda x: 'with basement' in x.lower())
    df['price_change'] = df['price'].astype(float) - df['price2'].astype(float)
    df['date_change'] = (df.date - df.date2).dt.days
    df['norm_price'] = ((1+(0.03/365)) ** (pd.datetime.today() - df['date']).dt.days) * df.price
    client.close()
    if save_csv: df.to_csv(meta_df.address.iloc[0] +".csv", index = False)
    results = (meta_df, df)
    return results #df

def filter_comps(df, price = 75000, turn_around = 400, t_time = pd.to_datetime('2018-01-01')):
    df_copy = df[
    (df['price_change']>=price) & 
    (df['date_change']<=turn_around) & 
    (df.date > pd.to_datetime(t_time))
    ]
    return df_copy


def find_comps(prop, update=True):
    if update:
        done_comps = []
    else:
        comps_df =  pd.read_csv('comps.csv')
        done_comps = comps_df.id.unique()        
    try:
        meta, comp = get_comps_sdat(prop)
        if comp.id.iloc[0] in done_comps:
            #do nothing
            print(prop, 'already done')
            filt_comp = filter_comps(comps_df.query('id == {}'.format(comps.id.iloc[0])))
            
        else:
            #load comp to csv
            filt_comp = filter_comps(comp)
            #filt_comp.to_csv('comps.csv', mode="a", header=False, index = False)

    except:
        search_address = prop.split()[:-1]
        search_address = ' '.join(search_address)
        try:
            if comp.id.iloc[0] in done_comps:
                #do nothing
                print(prop, 'already done')
            else:
                meta, comp = get_comps_sdat(search_address)
                filt_comp = filter_comps(comp, price=50000)
                #filt_comp.to_csv('comps.csv', mode="a", header=False, index = False)
        except:
            print('{}: Could not find property'.format(prop))
            return None
    
    results = (meta, filt_comp)    
    return results #filt_comp

def potential_property(f=None, df=None, address_col='address'):
    '''
    Function will create comps for each property in csv
    
    PARAMS:
        f: filename
        df: optional parameter to provide dataframe
        address_col: name of column that contains address
    '''
    if isinstance(df, pd.DataFrame): pass
    else: df = pd.read_csv(f)
    df.columns = df.columns.str.lower()
    address_col = address_col.lower()
    comps = pd.read_csv('comps.csv')
    done_comps = [] #comps.id.unique()
    
    properties = pd.read_csv('properties.csv')
    
    
    
    for prop in df[address_col].unique():
        comps = find_comps(prop)
        if isinstance(comps, pd.DataFrame):
            #do something
            vals = {'Address': [comps.id.iloc[0]],
                    'Re-Sale Price': [comps['price'].median()], 
                    'List Price': [df[df['address'] == prop]['current price'].mean()] 
                   }
            tmp_prop = pd.DataFrame(vals)
            tmp_prop['Margin'] = tmp_prop['Re-Sale Price'] - tmp_prop['List Price']
            properties = properties.append(tmp_prop, ignore_index=True)
            tmp_prop = None
        else:
            print(prop, 'could not be added')
        comps = None
    return properties
