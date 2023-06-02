package com.trunks.springbootbankinc.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.trunks.springbootbankinc.domain.Customer;
import com.trunks.springbootbankinc.domain.DocumentType;

@Repository
public interface CustomerRepository extends JpaRepository<Customer, Long>{

	@Query("select customer from Customer customer where customer.document =:document and customer.documentType =:documentType")
	Optional<Customer> findByDocTypeDoc(String document, DocumentType documentType);
}
