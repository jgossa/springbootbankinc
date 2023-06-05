package com.trunks.springbootbankinc.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.trunks.springbootbankinc.domain.Card;
import com.trunks.springbootbankinc.domain.Customer;
import com.trunks.springbootbankinc.domain.DocumentType;
import com.trunks.springbootbankinc.exception.BadRequestAlertException;
import com.trunks.springbootbankinc.repository.CardRepository;
import com.trunks.springbootbankinc.repository.CustomerRepository;

class CardServiceImplTest {

	@InjectMocks
	private CardServiceImpl cardService;
	
	@Mock
	private CustomerRepository customerRepository;
	
	@Mock
	private CardRepository cardRepository;
	 
    @BeforeEach
    public void setUp(){
        MockitoAnnotations.openMocks(this);
    }
    
    @Test
    void buidCard() {
    	String idCard = "1234566243116030";
    	String productid = "123456";
    	Customer customer = Customer.builder().build();
    	
    	assertNotNull(cardService.buidCard(idCard, productid, customer));
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
    	void esayWayWithCcTest() {
	    	String document = "18513058";
	    	String documentType = "CC";
	    	
	    	DocumentType finalDocType = DocumentType.CEDULACIUDADANIA;
	    	
	    	Customer customer = Customer.builder().id(Long.valueOf(1L)).build();
	    	Optional<Customer> optionalCustomer = Optional.of(customer);
	    	
	    	when(customerRepository.findByDocTypeDoc(document, finalDocType)).thenReturn(optionalCustomer);
    		
	    	assertNotNull(cardService.validateDbCustomer(documentType, document));
    	}
    	
    	@Test
    	void esayWayWithCeTest() {
	    	String document = "18513058";
	    	String documentType = "CE";
	    	
	    	DocumentType finalDocType = DocumentType.CEDULAEXTRANJERIA;
	    	
	    	Customer customer = Customer.builder().id(Long.valueOf(1L)).build();
	    	Optional<Customer> optionalCustomer = Optional.of(customer);
	    	
	    	when(customerRepository.findByDocTypeDoc(document, finalDocType)).thenReturn(optionalCustomer);
    		
	    	assertNotNull(cardService.validateDbCustomer(documentType, document));
    	}
    	
    	@Test
    	void userNotInDbExceptionTest() {
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
    
    @Nested
    class ValidateProductIdInCustomerCard {
    	
    	@Test
    	void easyWayTest() {
        	String productid = "123456";

        	Card card = Card.builder().idProducto("223456").build();
        	List<Card> cards = Arrays.asList(card);
        	
        	Customer customer = Customer.builder().cards(cards).build();
    		
    		cardService.validateProductIdInCustomerCard(productid, customer);
    		
    		assertEquals("123456", productid);
    	}
    	
    	@Test
    	void productIdPresenceExceptionTest() {
        	String productid = "123456";

        	Card card = Card.builder().idProducto("123456").build();
        	List<Card> cards = Arrays.asList(card);
        	
        	Customer customer = Customer.builder().cards(cards).build();
    		
	    	assertThrows(BadRequestAlertException.class, 
		    		() -> cardService.validateProductIdInCustomerCard(productid, customer));    	
	    }
    	
    	@Test
    	void cardIdProductNullTest() {
        	String productid = "123456";

        	Card card = Card.builder().idProducto(null).build();
        	List<Card> cards = Arrays.asList(card);
        	
        	Customer customer = Customer.builder().cards(cards).build();
        	
        	cardService.validateProductIdInCustomerCard(productid, customer);
        	
        	assertNull(customer.getCards().get(0).getIdProducto());
    	}
    }
    
    @Nested
    class ValidateCardInDb {
    	
    	@Test
    	void presentCardExceptionTest() {
    		String idCard = "1234566243116030";
    		
    		Card card = Card.builder().id(1L).number(idCard).build();
    		Optional<Card> optionalCard = Optional.of(card);
    		
    		when(cardRepository.findByCardNumber(idCard)).thenReturn(optionalCard);
    		
	    	assertThrows(BadRequestAlertException.class, 
		    		() -> cardService.validateCardInDb(idCard));    	
    	}
    	
    	@Test
    	void cardNotPresentTest() {
    		String idCard = "1234566243116030";
    		
    		Optional<Card> optionalCard = Optional.ofNullable(null);
    		
    		cardService.validateCardInDb(idCard);
    		
    		assertEquals("1234566243116030", idCard);
    	}
    } 
}
