package com.springvuegradle.Repositories;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;

import java.sql.*;

public class DBQueries {

    @Autowired
    private static Environment env;

    public static Long findIdByEmail(String email) throws SQLException {
        Long id = null;
        Statement stmt = null;
        String query = "SELECT p.id FROM Profile p JOIN profile_email pe ON p.id=pe.profile_id JOIN email e ON pe.email_id = e.id WHERE e.address = " + email;

        String database_url = "" + env.getProperty("spring.datasource.url") + "?user="
                + env.getProperty("spring.datasource.username") + "&" + env.getProperty("spring.datasource.password");
        System.out.println(database_url);
        try {

            Connection connection = DriverManager.getConnection(database_url);
            stmt = connection.createStatement();
            ResultSet rs = stmt.executeQuery(query);
            while (rs.next()) {
                id = rs.getLong("id");
            }
            stmt.close();
        } catch (Exception e) {
            System.err.println("hi" + e.getMessage());
        }
        return id;
    }


}
