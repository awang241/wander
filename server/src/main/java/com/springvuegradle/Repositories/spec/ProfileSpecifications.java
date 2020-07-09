package com.springvuegradle.Repositories.spec;

import com.springvuegradle.Model.Profile;
import com.springvuegradle.Model.Profile_;
import org.hibernate.query.criteria.internal.path.RootImpl;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

public class ProfileSpecifications {

    private ProfileSpecifications(){

    }

    public static Specification<Profile> profileContainsNickname() {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get(Profile_.nickname), "a");
    }

    public static Specification<Profile> profileContainsLastname() {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get(Profile_.lastname), "a");
    }

}
