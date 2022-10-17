

module ParseDivaXMLSpec where

import Test.Hspec


--------------------------------------------------------------------------------

import ParseDiva

{-


          <compensation>
            <compensation_coefficient>1.0</compensation_coefficient>
            <compensation_coefficient>0.0</compensation_coefficient>
            <compensation_coefficient>0.0</compensation_coefficient>
            <compensation_coefficient>0.0</compensation_coefficient>
            <compensation_coefficient>0.0</compensation_coefficient>
            <compensation_coefficient>0.0</compensation_coefficient>
            <compensation_coefficient>0.0</compensation_coefficient>
            <compensation_coefficient>0.0</compensation_coefficient>
          </compensation>


        <parameter name="FITC-A" type="30">
          <is_log>true</is_log>
          <is_quantitated>false</is_quantitated>
          <min>0.0</min>
          <max>5.4185380935668945</max>
          <raw_index>3</raw_index>
          <linear_index>3</linear_index>
          <log_index>21</log_index>
          <final_index>21</final_index>
          <fl>FITC</fl>
          <voltage>495</voltage>
          <target>3</target>
          <brightness>1.0</brightness>
          <threshold>5000</threshold>
          <trigger>false</trigger>
          <labels_only>false</labels_only>
          <biexp_scale>-1</biexp_scale>
          <comp_biexp_scale>-1</comp_biexp_scale>
          <manual_biexp_scale>0</manual_biexp_scale>
          <can_be_compensated>true</can_be_compensated>
          <compensation>
            <compensation_coefficient>1.0</compensation_coefficient>
            <compensation_coefficient>0.0</compensation_coefficient>
            <compensation_coefficient>0.0</compensation_coefficient>
            <compensation_coefficient>0.0</compensation_coefficient>
            <compensation_coefficient>0.0</compensation_coefficient>
            <compensation_coefficient>0.0</compensation_coefficient>
            <compensation_coefficient>0.0</compensation_coefficient>
            <compensation_coefficient>0.0</compensation_coefficient>
          </compensation>
        </parameter>


      <instrument_settings name="Cytometer Settings" template="false">
      </instrument_settings>


          <gate fullname="All Events\P1" type="SnapTo_Classifier">
            <name>P1</name>
            <color>0xff0000</color>
            <visible>true</visible>
            <enabled>true</enabled>
            <num_events>0</num_events>
            <parent>All Events</parent>
            <extended>
              <is_snap_mode>N</is_snap_mode>
              <point X="142318.0" Y="150421.0"/>
              <seed X="142318.0" Y="150421.0"/>
              <peak X="142318.0" Y="150421.0"/>
              <inspector_properties>
                <roaming auto_roaming_enabled="true" roaming_distance="18"/>
                <smoothing default_sigma_enabled="true" default_sigma="2.5" user_sigma="2.5"/>
              </inspector_properties>
            </extended>
            <region name="P1" xparm="FSC-A" yparm="SSC-A" type="POLYGON_REGION">
              <points>
                <point x="233540.07912776744" y="69198.420590873"/>
                <point x="233540.07912776744" y="17634.46274828058"/>
                <point x="247270.00599775926" y="-5999.017929574067"/>
                <point x="279722.56041773997" y="-5999.017929574067"/>
                <point x="293452.48728773184" y="19782.960991722182"/>
                <point x="293452.4872877318" y="73495.41707775582"/>
                <point x="279722.56041773997" y="97128.89775561061"/>
                <point x="249766.35633775784" y="97128.89775561061"/>
              </points>
            </region>
            <is_x_parameter_scaled>false</is_x_parameter_scaled>
            <is_y_parameter_scaled>false</is_y_parameter_scaled>
            <is_x_parameter_log>false</is_x_parameter_log>
            <is_y_parameter_log>false</is_y_parameter_log>
            <x_parameter_scale_value>-1</x_parameter_scale_value>
            <y_parameter_scale_value>-1</y_parameter_scale_value>
            <input>All Events</input>
          </gate>


        <gates>
        </gates>


-}



spec :: Spec
spec = describe "Parse DIVA XML components" $ do
  it "parse compensation entry" $ do
    --parse known_marker "" "CD4" `shouldParse` "CD4"
    False `shouldBe` True

