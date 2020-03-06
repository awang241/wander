package com.springvuegradle.Utiilities;

import com.springvuegradle.Model.PassportCountry;

import java.io.IOException;
import java.net.ConnectException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ValidationHelper {

    /**
     * Connects to restcountries API and obtains a string of every countries name in a JSON Object format
     * Cuts around JSON Objects to get the name of each country and places them into a returned array "countries"
     * @throws IOException (URL Type needs IOException for whatever reason)
     * @author Matthew Wong
     */
    public static List<String> GetRESTCountries() throws IOException {
        URL restCountries = new URL("https://restcountries.eu/rest/v2/all?fields=name");
        HttpURLConnection connection = (HttpURLConnection) restCountries.openConnection();
        connection.setRequestMethod("GET");
        List<String> countries = new ArrayList<String>();
        try (java.io.InputStream in = new java.net.URL("https://restcountries.eu/rest/v2/all?fields=name").openStream()) {
            // Get the names of REST countries API
            String names = new String(in.readAllBytes());
            // Trim down the [] characters in the string names
            names = names.substring(1, names.length()-1);
            countries = Arrays.asList(names.split(","));
            // Get each countries name alone and place them into an array "countries"
            for (int i = 0; i < countries.size(); i++) {
                String country = countries.get(i);
                countries.set(i, country.substring(9, country.length() - 2));
            }
        } catch(ConnectException exception) {
            throw exception;
        }
        return countries;

    }

    public static boolean validateCountry(PassportCountry testCountry, List<String> countries) {
        boolean test = false;
        for (String country : countries) {
            if(country.equals(testCountry.getCountryName())) {
                test = true;
                break;
            }
        }
        return test;
    }

}