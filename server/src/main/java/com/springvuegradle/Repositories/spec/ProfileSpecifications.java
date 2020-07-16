package com.springvuegradle.Repositories.spec;

import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Model.Email;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Model.Profile_;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.Join;
import javax.persistence.metamodel.SingularAttribute;
import java.beans.Expression;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

/**
 * Helper class containing methods to create Specifications to use in ProfileRepository. Specifications can be passed to
 * findAll() to filter results. The specifications can be chained to create more complex criteria (e.g. (nickname matches
 * pattern X and firstname matches pattern Y) or lastname matches pattern Z); see the Specification javadoc page.
 */
public class ProfileSpecifications {

    /**
     * Private constructor for class.
     */
    private ProfileSpecifications() {
    }

    public static Specification<Profile> notDefaultAdmin() {
        return (root, query, criteriaBuilder) -> criteriaBuilder.greaterThan(root.get("authLevel"), 0);
    }

    public static Specification<Profile> firstNameContains(String substring) {
        String pattern = "%" + substring.toLowerCase() + "%";
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(criteriaBuilder.lower(root.get("firstname")), pattern);
    }

    public static Specification<Profile> middleNameContains(String substring) {
        String pattern = "%" + substring.toLowerCase() + "%";
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(criteriaBuilder.lower(root.get("middlename")), pattern);
    }

    public static Specification<Profile> nicknameContains(String substring) {
        String pattern = "%" + substring.toLowerCase() + "%";
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(
                criteriaBuilder.lower(root.get("nickname")), pattern);
    }

    public static Specification<Profile> lastNameContains(String substring) {
        String pattern = "%" + substring.toLowerCase() + "%";
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(
                criteriaBuilder.lower(root.get("lastname")), pattern);
    }

    public static Specification<Profile> stringFieldContains(SingularAttribute<Profile, String> field, String substring) {
        String pattern = "%" + substring.toLowerCase() + "%";
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(
                criteriaBuilder.lower(root.get(field)), pattern);
    }

    public static Specification<Profile> hasEmail(Email email) {
        String pattern = "%" + email.getAddress().toLowerCase() + "%";
        return (root, query, criteriaBuilder) -> {
            Join<Profile, Email> profileEmailJoin = root.join("emails");
            return criteriaBuilder.like(criteriaBuilder.lower(profileEmailJoin.get("address")), pattern);
        };
    }

    /**
     * Creates a specification to match profiles that have the provided activity types.
     * @param types The collection of activity types to me matched against.
     * @param searchMethod true if only profiles that match all activity types should be provided. If false, any profile
     *                     that has any of the types given will be returned.
     * @return a specification that matches profiles that have the provided activity types.
     */
    public static Specification<Profile> activityTypesContains(String[] types, String searchMethod) {
        Specification<Profile> spec = Specification.where(null);
        for(int i = 0; i < types.length; i++) {
            final String type = types[i];
            Specification<Profile> activitySpec = (root, query, criteriaBuilder) -> {
                Join<Profile, ActivityType> groupJoin = root.join(Profile_.activityTypes);
                return criteriaBuilder.equal(groupJoin.get("activityTypeName"), type);
            };

            if (searchMethod.equals("all")) {
                spec = spec.and(activitySpec);
            } else {
                spec = spec.or(activitySpec);
            }
        }
        return spec;
    }
}
