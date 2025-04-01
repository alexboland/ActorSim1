import React, { useState } from 'react';
import styles from './ResourcesPanel.module.css';

const ResourcesPanel = ({ government }) => {
  const [selectedResource, setSelectedResource] = useState('Food');
  const [bidPrice, setBidPrice] = useState(1);
  const [askPrice, setAskPrice] = useState(1);

  const resourceTypes = ['Food', 'Wood', 'Copper'];

  const updatePrice = async (priceType) => {
    try {
      const response = await fetch(`http://localhost:8080/government/setPrice`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          priceType: priceType,
          resourceType: selectedResource,
          price: priceType === 'bid' ? bidPrice : askPrice
        })
      });

      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }

      // Provide feedback to user
      alert(`${priceType.charAt(0).toUpperCase() + priceType.slice(1)} price for ${selectedResource} updated successfully`);
    } catch (error) {
      console.error(`Failed to update ${priceType} price:`, error);
      alert(`Failed to update ${priceType} price: ${error.message}`);
    }
  };

  return (
    <div className={styles.resourcesPanel}>
      <h3 className={styles.sectionTitle}>Government Resources</h3>

      <div className={styles.resourceStatus}>
        <h4>Stored Resources</h4>
        <div className={styles.resourcesGrid}>
          {government.storedResources && Object.entries(government.storedResources).map(([resource, amount]) => (
            <div key={resource} className={styles.resourceItem}>
              <span className={styles.resourceName}>{resource}:</span>
              <span className={styles.resourceAmount}>{amount}</span>
            </div>
          ))}
        </div>
      </div>

      <div className={styles.priceControls}>
        <h4>Set Resource Prices</h4>

        <div className={styles.resourceSelector}>
          <label>Resource Type:</label>
          <select
            value={selectedResource}
            onChange={(e) => setSelectedResource(e.target.value)}
            className={styles.select}
          >
            {resourceTypes.map(type => (
              <option key={type} value={type}>{type}</option>
            ))}
          </select>
        </div>

        <div className={styles.priceControl}>
          <h5>Bid Price (Government Buying)</h5>
          <div className={styles.controlRow}>
            <input
              type="number"
              min="0"
              value={bidPrice}
              onChange={(e) => setBidPrice(Number(e.target.value))}
              className={styles.input}
            />
            <button
              onClick={() => updatePrice('bid')}
              className={styles.button}
            >
              Set Bid Price
            </button>
          </div>
          <div className={styles.currentPrice}>
            Current Bid Price: {government.bidPrices && government.bidPrices[selectedResource] ? government.bidPrices[selectedResource] : 'Not set'}
          </div>
        </div>

        <div className={styles.priceControl}>
          <h5>Ask Price (Government Selling)</h5>
          <div className={styles.controlRow}>
            <input
              type="number"
              min="0"
              value={askPrice}
              onChange={(e) => setAskPrice(Number(e.target.value))}
              className={styles.input}
            />
            <button
              onClick={() => updatePrice('ask')}
              className={styles.button}
            >
              Set Ask Price
            </button>
          </div>
          <div className={styles.currentPrice}>
            Current Ask Price: {government.askPrices && government.askPrices[selectedResource] ? government.askPrices[selectedResource] : 'Not set'}
          </div>
        </div>
      </div>
    </div>
  );
};

export default ResourcesPanel;