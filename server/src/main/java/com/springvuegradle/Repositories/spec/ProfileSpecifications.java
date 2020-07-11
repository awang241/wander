package com.springvuegradle.Repositories.spec;

import com.springvuegradle.Model.ActivityType;
import com.springvuegradle.Model.Email;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Model.Profile_;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.metamodel.SingularAttribute;
import java.util.Collection;
import java.util.Iterator;

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
        return (root, query, criteriaBuilder) -> criteriaBuilder.greaterThan(root.get(Profile_.authLevel), 0);
    }

    public static Specification<Profile> firstNameContains(String substring) {
        String pattern = "%" + substring + "%";
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get(Profile_.firstname), pattern);
    }

    public static Specification<Profile> middleNameContains(String substring) {
        String pattern = "%" + substring + "%";
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get(Profile_.middlename), pattern);
    }

    public static Specification<Profile> nicknameContains(String substring) {
        String pattern = "%" + substring + "%";
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get(Profile_.nickname), pattern);
    }

    public static Specification<Profile> lastNameContains(String substring) {
        String pattern = "%" + substring + "%";
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get(Profile_.lastname), pattern);
    }

    public static Specification<Profile> stringFieldContains(SingularAttribute<Profile, String> field, String substring) {
        String pattern = "%" + substring + "%";
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get(field), pattern);
    }

    public static Specification<Profile> hasEmail(Email email) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.isMember(email, root.get(Profile_.emails));
    }

    /**
     * Creates specification to match profiles that have the provided activity types.
     * @param types The collection of activity types to me matched against.
     * @param matchAll true if only profiles that match all activity types should be provided. If false, any profile that
     *                 has any of the types given will be returned.
     * @return a specification that matches profiles that have the provided activity types.
     */
    public static Specification<Profile> activityTypesContains(Collection<ActivityType> types, Boolean matchAll) {
        Specification<Profile> spec = Specification.where(null);
        Iterator<ActivityType> iterator = types.iterator();
        while (iterator.hasNext()) {
            Specification<Profile> activitySpec = (root, query, criteriaBuilder) ->
                    criteriaBuilder.isMember(types.iterator().next(), root.get(Profile_.activityTypes));
            if (Boolean.TRUE.equals(matchAll)) {
                spec = spec.and(activitySpec);
            } else {
                spec = spec.or(activitySpec);
            }
        }
        return spec;
    }
}
