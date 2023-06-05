package com.trunks.springbootbankinc.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

import java.util.Optional;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.trunks.springbootbankinc.domain.Customer;
import com.trunks.springbootbankinc.domain.DocumentType;
import com.trunks.springbootbankinc.exception.BadRequestAlertException;
import com.trunks.springbootbankinc.repository.CustomerRepository;

class CardServiceImplTest {

	@InjectMocks
	private CardServiceImpl cardService;
	
	@Mock
	private CustomerRepository customerRepository;
	 
    @BeforeEach
    public void setUp(){
        MockitoAnnotations.openMocks(this);
    }
	
    @Nested
    class validateCardNumberParams{
    
	    @Test
	    void easyWayTest() {
	    	
	    	String productId = "123456";
	    	String document = "18513058";
	    	String documentType = "CC";
	    	
	    	assertEquals("123456", productId);
	    	
	    	cardService.validateCardNumberParams(productId, document, documentType);
	    }
	    
	    @Test
	    void productIdExceptionTest() {
	    	String productId = "12345";
	    	String document = "A8513058";
	    	String documentType = "CC";
	    	
	    	assertThrows(BadRequestAlertException.class, 
	    		() -> cardService.validateCardNumberParams(productId, document, documentType));
	    }
	    
	    @Test
	    void documentExceptionTest() {
	    	String productId = "123456";
	    	String document = "18513058A";
	    	String documentType = "CC";
	    	
	    	assertThrows(BadRequestAlertException.class, 
	    		() -> cardService.validateCardNumberParams(productId, document, documentType));
	    }
	    
	    @Test
	    void typeDocumentExceptionTest() {
	    	String productId = "123456";
	    	String document = "18513058";
	    	String documentType = "CK";
	    	
	    	assertThrows(BadRequestAlertException.class, 
	    		() -> cardService.validateCardNumberParams(productId, document, documentType));
	    }
    }
    
    @Nested
    class ValidateDbCustomer {
    	
    	@Test
    	void esayWayWithCC() {
	    	String document = "18513058";
	    	String documentType = "CC";
	    	
	    	DocumentType finalDocType = DocumentType.CEDULACIUDADANIA;
	    	
	    	Customer customer = Customer.builder().id(Long.valueOf(1L)).build();
	    	Optional<Customer> optionalCustomer = Optional.of(customer);
	    	
	    	when(customerRepository.findByDocTypeDoc(document, finalDocType)).thenReturn(optionalCustomer);
    		
	    	assertNotNull(cardService.validateDbCustomer(documentType, document));
    	}
    	
    	@Test
    	void esayWayWithCE() {
	    	String document = "18513058";
	    	String documentType = "CE";
	    	
	    	DocumentType finalDocType = DocumentType.CEDULAEXTRANJERIA;
	    	
	    	Customer customer = Customer.builder().id(Long.valueOf(1L)).build();
	    	Optional<Customer> optionalCustomer = Optional.of(customer);
	    	
	    	when(customerRepository.findByDocTypeDoc(document, finalDocType)).thenReturn(optionalCustomer);
    		
	    	assertNotNull(cardService.validateDbCustomer(documentType, document));
    	}
    	
    	@Test
    	void userNotInDbException() {
	    	String document = "18513058";
	    	String documentType = "CE";
	    	
	    	DocumentType finalDocType = DocumentType.CEDULACIUDADANIA;
	    	
	    	Customer customer = Customer.builder().id(Long.valueOf(1L)).build();
	    	Optional<Customer> optionalCustomer = Optional.of(customer);
	    	
	    	when(customerRepository.findByDocTypeDoc(document, finalDocType)).thenReturn(optionalCustomer);
    		
	    	assertThrows(BadRequestAlertException.class, 
		    		() -> cardService.validateDbCustomer(documentType, document));
    	}
    }
}
