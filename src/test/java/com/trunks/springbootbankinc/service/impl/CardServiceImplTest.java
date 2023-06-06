package com.trunks.springbootbankinc.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Date;
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
import com.trunks.springbootbankinc.domain.TransactionHistory;
import com.trunks.springbootbankinc.domain.TransactionType;
import com.trunks.springbootbankinc.dto.CardAnulationTransanctioDTO;
import com.trunks.springbootbankinc.dto.CardPurchaseDTO;
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
	
	@Mock
	private List<TransactionHistory> transactionHistories;
	 
    @BeforeEach
    public void setUp(){
        MockitoAnnotations.openMocks(this);
    }
    
    @Test
    void saveTest() {
    	Card card = Card.builder().build();
    	
    	when(cardRepository.save(card)).thenReturn(new Card());
    	
    	assertNotNull(cardService.save(card));
    }
    
    @Test
    void findCardByTransactionNumberTest() {
    	String transactionId = "bfe6e24a50d77c6385a98faac2fd85972b4d2f9c5bdaea7a9fb5cac834c8c057";
    	
    	Optional<Card> optionalCard = Optional.ofNullable(new Card());
    	
    	when(cardRepository.findCardByTransactionNumber(transactionId)).thenReturn(optionalCard);
    	
    	assertNotNull(cardService.findCardByTransactionNumber(transactionId));
    }
    
    @Nested
    class BuilderEntity {
    
	    @Test
	    void buildCardTrxHistoryTest() {
	    	Card card = Card.builder().build(); 
	    	TransactionType transactionType = TransactionType.CARDACTIVATION;
	    	boolean enroll = true; 
	    	boolean block = false;
	    	Float balance = 0.0F; 
	    	Float trxAmmount = 0.0F;
	    	
	    	assertNotNull(cardService.buildCardTrxHistory(card, transactionType, enroll, block, balance, trxAmmount));
	    }
	    
	    @Test
	    void buidCard() {
	    	String idCard = "1234566243116030";
	    	String productid = "123456";
	    	Customer customer = Customer.builder().build();
	    	
	    	assertNotNull(cardService.buidCard(idCard, productid, customer));
	    }
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
    		
    		when(cardRepository.findByCardNumber(idCard)).thenReturn(optionalCard);
    		
    		cardService.validateCardInDb(idCard);
    		
    		assertEquals("1234566243116030", idCard);
    	}
    } 
    
    @Nested
    class BuildTransactionDTOResponse {
    	
    	@Test
    	void easyWayTest() {
    		String transactionId = "bfe6e24a50d77c6385a98faac2fd85972b4d2f9c5bdaea7a9fb5cac834c8c057";
    		
    		TransactionHistory trxHistory = TransactionHistory.builder()
    				.id(1L)
    				.date(new Date())
    				.transactionNumber("bfe6e24a50d77c6385a98faac2fd85972b4d2f9c5bdaea7a9fb5cac834c8c057")
    				.accountBalance(0.0F)
    				.transactionAmmount(0.0F)
    				.transactionType(TransactionType.REVERSEDTRANSACTION)
    				.enroll(true)
    				.block(false)
    				.build();
    		
    		List<TransactionHistory> trxHistories = Arrays.asList(trxHistory);
    		
    		Card card = Card.builder()
    				.id(1L)
    				.transactionHistories(trxHistories)
    				.build();    	
    		
    		assertNotNull(cardService.buildTransactionDTOResponse(card, transactionId));
    	}
    	
       	@Test
    	void transactionNumberExceptionTest() {
    		String transactionId = "bfe6e24a50d77c6385a98faac2fd85972b4d2f9c5bdaea7a9fb5cac834c8c057";
    		
    		TransactionHistory trxHistory = TransactionHistory.builder()
    				.id(1L)
    				.date(new Date())
    				.transactionNumber(null)
    				.accountBalance(0.0F)
    				.transactionAmmount(0.0F)
    				.transactionType(TransactionType.REVERSEDTRANSACTION)
    				.enroll(true)
    				.block(false)
    				.build();
    		
    		List<TransactionHistory> trxHistories = Arrays.asList(trxHistory);
    		
    		Card card = Card.builder()
    				.id(1L)
    				.transactionHistories(trxHistories)
    				.build();   
    		
	    	assertThrows(BadRequestAlertException.class, 
		    		() -> cardService.buildTransactionDTOResponse(card, transactionId));    	
    	}
    }
    
    @Nested
    class BuildPurchaseTrxInfoDTO {
    	
    	@Test 
    	void easyWayTest() {
    		Card card = Card.builder()
    				.number("1234566243116030")
    				.accountBalance(Float.valueOf(1000000F))
    				.build();
    		
    		CardPurchaseDTO cardPurchaseDTO = CardPurchaseDTO.builder()
    				.price(Float.valueOf(5000.0F))
    				.cardId("1234566243116030")
    				.build();
    		
    		assertNotNull(cardService.buildPurchaseTrxInfoDTO(card, cardPurchaseDTO));
    	}
    	
    	@Test 
    	void insufficientBalanceExceptionTest() {
    		Card card = Card.builder()
    				.number("1234566243116030")
    				.accountBalance(Float.valueOf(1000F))
    				.build();
    		
    		CardPurchaseDTO cardPurchaseDTO = CardPurchaseDTO.builder()
    				.price(Float.valueOf(1000000F))
    				.cardId("1234566243116030")
    				.build();

	    	assertThrows(BadRequestAlertException.class, 
		    		() -> cardService.buildPurchaseTrxInfoDTO(card, cardPurchaseDTO));    	
    	}
    }
    
    @Nested
    class BuildAnulationTrxInfoDTO {
    	
    	@Test
    	void reversedTransactionExceptionTest() {
    		String transactionId = "bfe6e24a50d77c6385a98faac2fd85972b4d2f9c5bdaea7a9fb5cac834c8c057";
    		
    		TransactionHistory trxHistory = TransactionHistory.builder()
    				.transactionNumber("bfe6e24a50d77c6385a98faac2fd85972b4d2f9c5bdaea7a9fb5cac834c8c057")
    				.accountBalance(1000000F)
    				.transactionAmmount(50000F)
    				.date(new Date())
    				.transactionType(TransactionType.REVERSEDTRANSACTION)
    				.build();
    		
    		List<TransactionHistory> trxHistories = Arrays.asList(trxHistory);
    		
    		Card card = Card.builder()
    				.transactionHistories(trxHistories)
    				.build();   
    		
    		CardAnulationTransanctioDTO cardAnulationTransanctioDTO = CardAnulationTransanctioDTO.builder()
    				.transactionId(transactionId)
    				.build();
    		
	    	assertThrows(BadRequestAlertException.class, 
		    		() -> cardService.buildAnulationTrxInfoDTO(card, cardAnulationTransanctioDTO));    	
    	}
    	
    	@Test
    	void wrongDateExceptionTest() {
    		String transactionId = "bfe6e24a50d77c6385a98faac2fd85972b4d2f9c5bdaea7a9fb5cac834c8c057";
    		
			long longCurrentDate = new Date().getTime();
			long longFutureDate = longCurrentDate + (25 * 60 * 60 * 1000); // 25 hours in milliseconds
			Date futureDate = new Date(longFutureDate); // Current Date + 25 hours
    		
    		TransactionHistory trxHistory = TransactionHistory.builder()
    				.transactionNumber("bfe6e24a50d77c6385a98faac2fd85972b4d2f9c5bdaea7a9fb5cac834c8c057")
    				.accountBalance(1000000F)
    				.transactionAmmount(50000F)
    				.date(futureDate)
    				.transactionType(TransactionType.ANULATIONTRANSACTION)
    				.build();
    		
    		List<TransactionHistory> trxHistories = Arrays.asList(trxHistory);
    		
    		Card card = Card.builder()
    				.transactionHistories(trxHistories)
    				.build();   
    		
    		CardAnulationTransanctioDTO cardAnulationTransanctioDTO = CardAnulationTransanctioDTO.builder()
    				.transactionId(transactionId)
    				.build();
    		
	    	assertThrows(BadRequestAlertException.class, 
		    		() -> cardService.buildAnulationTrxInfoDTO(card, cardAnulationTransanctioDTO));    	
    	}
    }
}
