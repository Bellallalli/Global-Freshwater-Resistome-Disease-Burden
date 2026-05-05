import pandas as pd
import numpy as np
from collections import Counter
from sklearn.linear_model import ElasticNet
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler, KBinsDiscretizer

# ==========================================
# 1. 数据准备
# ==========================================
file_path = r"F:\Global Samples\Model Data\2026-01-12 Economy-4dataset\7Train-3Test-Final\02.dataset\02.4Economy-RM-Trainset.xlsx"
df = pd.read_excel(file_path, index_col=0)

mandatory_cols = df.columns[0:15].tolist()
candidate_cols = df.columns[15:-2].tolist()
target_col = df.columns[-1]

# 预过滤 (Prevalence > 10%)
prevalence = (df[candidate_cols] > 0).sum(axis=0) / len(df)
filtered_candidates = prevalence[prevalence >= 0.10].index.tolist()

# 数据标准化
scaler = StandardScaler()
X_mandatory = scaler.fit_transform(np.log1p(df[mandatory_cols].astype(float)))
X_candidates = scaler.fit_transform(np.log1p(df[filtered_candidates].astype(float)))
y = df[target_col].values.reshape(-1, 1)  # 回归 y 通常不需要转整数

# --- 核心修改：为回归 y 创建采样"区间" ---
# 将 y 分成 3 个区间（低、中、高），用于分层抽样
kbd = KBinsDiscretizer(n_bins=3, encode='ordinal', strategy='quantile')
y_bins = kbd.fit_transform(y).ravel()

X_combined = np.hstack([X_mandatory, X_candidates])


# ==========================================
# 2. 回归版稳定性选择函数
# ==========================================
def run_regression_elastic_net_stability(X, y, y_bins, mandatory_count, cand_names, n_iter=1000):
    counts = Counter()

    print(f"开始执行 {n_iter} 次回归 Elastic Net 重采样...")
    for i in range(n_iter):
        # 在 y_bins 的基础上进行分层抽样，确保回归值的分布稳定
        X_train, _, y_train, _ = train_test_split(
            X, y.ravel(), test_size=0.2, stratify=y_bins, random_state=i
        )

        # 建立回归 ElasticNet 模型
        # alpha 对应惩罚强度（等同于之前的 1/C），l1_ratio=0.5
        # 对应文章 lambda=0.01 的要求
        model = ElasticNet(alpha=0.01, l1_ratio=0.5, max_iter=5000, random_state=i)
        model.fit(X_train, y_train)

        # 提取待筛选特征部分的系数
        cand_coefs = model.coef_[mandatory_count:]

        # ============================================
        # 关键修改：保留所有显著的非零系数（正负都保留）
        # 将原来的 cand_coefs > 0 改为 abs(cand_coefs) > threshold
        # ============================================
        selected_idx = np.where(np.abs(cand_coefs) > 1e-6)[0]  # 使用绝对值判断
        selected_names = [cand_names[j] for j in selected_idx]
        counts.update(selected_names)

        if (i + 1) % 100 == 0:
            print(f"进度: {i + 1} / {n_iter}")

    return counts


# 执行筛选
feature_counts = run_regression_elastic_net_stability(
    X_combined, y, y_bins,
    len(mandatory_cols),
    filtered_candidates,
    n_iter=1000
)

# ==========================================
# 3. 结果汇总与导出
# ==========================================
res_df = pd.DataFrame(feature_counts.most_common(), columns=['Feature', 'Frequency'])
res_df['Selection_Probability'] = res_df['Frequency'] / 1000

# 强制保留特征置顶
mandatory_df = pd.DataFrame({
    'Feature': mandatory_cols,
    'Frequency': [1000] * len(mandatory_cols),
    'Selection_Probability': [1.0] * len(mandatory_cols)
})

final_output = pd.concat([mandatory_df, res_df], axis=0).reset_index(drop=True)
output_path = r"F:\Global Samples\Model Data\2026-01-12 Economy-4dataset\7Train-3Test-Final\03.ElasticNet\RM.xlsx"
final_output.to_excel(output_path, index=False)

print(f"\n--- 任务完成 ---")
print(f"回归模型筛选结果已保存至: {output_path}")
print(f"\n筛选统计:")
print(f"  强制特征: {len(mandatory_cols)}个")
print(f"  候选特征中稳定选择: {len(res_df)}个")
print(f"  总特征: {len(final_output)}个")
print(f"  预过滤保留(Prevalence > 10%): {len(filtered_candidates)}个")