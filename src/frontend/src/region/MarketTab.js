import React, { useState } from 'react';
import styles from './MarketTab.module.css';

const MarketTab = ({ market }) => {
  const [selectedView, setSelectedView] = useState('overview');

  if (!market) {
    return <div>No market data available</div>;
  }

  const topBids = market.topBids || {};
  const topAsks = market.topAsks || {};

  // Get unique resources from both bids and asks
  const uniqueResources = [...new Set([
    ...Object.keys(topBids),
    ...Object.keys(topAsks)
  ])];

  // Placeholder for orders (would come from real data in production)
  const myOrders = [
    // Placeholder data
  ];

  const renderMarketOverview = () => {
    if (uniqueResources.length === 0) {
      return <div className={styles.emptyMessage}>No market activity at this time</div>;
    }

    return (
      <div className={styles.marketOverviewContainer}>
        <div className={styles.tableContainer}>
          <table className={styles.table}>
            <thead>
              <tr>
                <th className={styles.tableHeaderCell}>Resource</th>
                <th className={styles.tableHeaderCellRight}>Top Bid</th>
                <th className={styles.tableHeaderCellRight}>Top Ask</th>
                <th className={styles.tableHeaderCellRight}>Spread</th>
              </tr>
            </thead>
            <tbody>
              {uniqueResources.map(resource => {
                const topBid = topBids[resource] || '-';
                const topAsk = topAsks[resource] || '-';
                const spread = topBid !== '-' && topAsk !== '-'
                  ? (topAsk - topBid).toFixed(2)
                  : '-';

                return (
                  <tr key={resource} className={styles.tableRow}>
                    <td className={styles.tableCell}>{resource}</td>
                    <td className={styles.tableCellRight}>
                      {topBid !== '-' ? topBid : '-'}
                    </td>
                    <td className={styles.tableCellRight}>
                      {topAsk !== '-' ? topAsk : '-'}
                    </td>
                    <td className={styles.tableCellRight}>{spread}</td>
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
      </div>
    );
  };





  return (
    <div className={styles.marketTabContainer}>
      <div className={styles.marketHeader}>
        <div className={styles.marketId}>
          Market ID: {market.id ? market.id.slice(0, 8) : 'Unknown'}
        </div>
        <div className={styles.marketStatus}>
          <span className={styles.statusIndicator}></span>
          Active
        </div>
      </div>

      <div className={styles.contentSection}>
        {renderMarketOverview()}
      </div>
    </div>
  );
};

export default MarketTab;