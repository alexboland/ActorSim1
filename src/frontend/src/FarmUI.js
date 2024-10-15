import React from 'react';

const FarmUI = ({ farm }) => {
  return (
    <div style={styles.card}>
      <h3 style={styles.title}>Farm</h3>
      <p>Workers: {farm.workers}</p>
      <p>Production: {farm.production}</p>
      <p>Stored Food: {farm.storedFood}</p>
      {/* Add more farm-specific attributes here */}
    </div>
  );
};

const styles = {
  card: {
    backgroundColor: '#ffffff',
    borderRadius: '8px',
    padding: '15px',
    margin: '10px 0',
    boxShadow: '0 2px 4px rgba(0,0,0,0.1)',
  },
  title: {
    fontSize: '1.2em',
    marginBottom: '10px',
    color: '#4a5568',
  },
};

export default FarmUI;